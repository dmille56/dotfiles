"""Shared backend selection and tmux transport for Plannotator skills/hooks."""

import fcntl
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile

BROWSER = "@BROWSER@"
TUI = "@TUI@"
TMUX = "@TMUX@"
BUN = "@BUN@"
RESOLVER = "@RESOLVER@"
SETTING = Path("@SETTING@")


def browser_mode():
    try:
        return SETTING.read_text().strip() != "false"
    except (OSError, UnicodeError):
        return True


def mode(args):
    if args == ["status"]:
        print("browser" if browser_mode() else "tui")
        return 0
    if args not in (["browser"], ["tui"]):
        print("Usage: plannotator-mode browser|tui|status", file=sys.stderr)
        return 2
    SETTING.parent.mkdir(parents=True, exist_ok=True)
    # Rename within the same directory: readers always see a complete value.
    with tempfile.NamedTemporaryFile(mode="w", dir=SETTING.parent, delete=False) as out:
        temporary = Path(out.name)
        out.write("true\n" if args == ["browser"] else "false\n")
    try:
        os.replace(temporary, SETTING)
    finally:
        temporary.unlink(missing_ok=True)
    print(args[0])
    return 0


def run(command, **kwargs):
    return subprocess.run(command, check=True, **kwargs)


def tmux_context():
    """Choose only a client currently displaying the invoking agent's pane."""
    context = os.environ.get("TMUX", "").rsplit(",", 2)
    pane = os.environ.get("TMUX_PANE", "")
    if len(context) != 3 or not context[0] or not pane:
        return None
    command = [TMUX, "-S", context[0]]
    try:
        window = run(command + ["display-message", "-p", "-t", pane, "#{window_id}"],
                     capture_output=True, text=True).stdout.strip()
        clients = run(command + ["list-clients", "-F",
                      "#{client_name}\t#{session_id}\t#{window_id}\t#{client_activity}"],
                      capture_output=True, text=True).stdout.splitlines()
        # Prefer the invoking session when a window is linked into multiple sessions.
        candidates = []
        for client in clients:
            name, session, visible_window, activity = client.split("\t")
            if visible_window == window:
                candidates.append((session == "$" + context[2], int(activity or 0), name))
        if not candidates:
            return None
        return command, max(candidates)[2]
    except (OSError, ValueError, subprocess.CalledProcessError):
        return None


def supported_target(args):
    if len(args) != 2 or args[0] != "annotate":
        return None
    path = Path(args[1]).absolute()
    if path.is_dir() or (path.is_file() and path.suffix.lower() in (".md", ".markdown", ".mdown", ".mkd")):
        return path
    return None


def popup_worker(root):
    """Run on the popup's terminal; publish feedback only after successful exit."""
    root = Path(root)
    request = json.loads((root / "request.json").read_text())
    env = os.environ.copy()
    # The tmux server can have a different environment from the calling agent.
    for name, value in request["environment"].items():
        if value is None:
            env.pop(name, None)
        else:
            env[name] = value
    for name in list(env):
        if name.startswith("HERDR_"):
            del env[name]
    env["PLANNOTATOR_DATA_DIR"] = str(root / "data")
    run([TUI, request["target"]], env=env)
    feedback = run([TUI, "--export", request["target"]], env=env,
                   capture_output=True, text=True).stdout.strip()
    if feedback == "No annotations.":
        feedback = ""
    (root / "completed.json").write_text(json.dumps({"feedback": feedback}))
    return 0


def popup(context, root, target):
    command, client = context
    environment = {name: os.environ.get(name) for name in (
        "HOME", "PATH", "XDG_CONFIG_HOME", "CODEX_HOME", "CODEX_THREAD_ID",
        "PLANNOTATOR_TUI_CONFIG", "PLANNOTATOR_TUI_THEME",
    )}
    (root / "request.json").write_text(json.dumps({
        "target": str(target), "environment": environment,
    }))
    worker = shlex.join([sys.executable, str(Path(__file__).resolve()),
                        "--popup-worker", str(root)])
    # Serialize popups per client; simultaneous agents must not replace each other.
    lock_dir = Path(tempfile.gettempdir()) / f"plannotator-popup-{os.getuid()}"
    lock_dir.mkdir(mode=0o700, exist_ok=True)
    import hashlib
    lock_name = hashlib.sha256(shlex.join(command + [client]).encode()).hexdigest()
    with (lock_dir / lock_name).open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        run(command + ["display-popup", "-E", "-c", client, "-T", "Plannotator",
                       "-w", "90%", "-h", "90%", "-d", os.getcwd(), worker],
            capture_output=True, text=True)
    # A dismissed popup can return success without completing the worker.
    return json.loads((root / "completed.json").read_text())["feedback"]


def browser(args, event):
    if event is None:
        os.execv(BROWSER, [BROWSER, *args])
    return subprocess.run([BROWSER, *args], input=event).returncode


def launch(args):
    event = sys.stdin.buffer.read() if not args else None
    context = None if browser_mode() else tmux_context()
    if context is None:
        return browser(args, event)
    target = supported_target(args)
    if args and args != ["last"] and target is None:
        return browser(args, event)
    try:
        with tempfile.TemporaryDirectory(prefix="plannotator-review-") as directory:
            root = Path(directory)
            if not args:
                plan = run([BUN, RESOLVER], input=event, capture_output=True).stdout.decode().strip()
                if not plan:
                    return 0  # The existing hook's resolver found no new plan.
                target = root / "plan.md"
                target.write_text(plan)
            elif args == ["last"]:
                # Resolve before opening: the popup is not descended from the agent.
                reply = run([TUI, "last", "--host", "codex", "--print"],
                            capture_output=True, text=True).stdout.strip()
                if not reply:
                    raise ValueError("could not resolve the current Codex response")
                target = root / "last-response.md"
                target.write_text(reply)
            feedback = popup(context, root, target)
            if event is not None:
                print(json.dumps({"decision": "block", "reason": feedback} if feedback else {}))
            elif feedback:
                print(feedback)
            return 0
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"Plannotator popup unavailable; opening browser ({error}).", file=sys.stderr)
        return browser(args, event)


def main(args):
    if args[:1] == ["--mode"]:
        return mode(args[1:])
    if args[:1] == ["--popup-worker"]:
        return popup_worker(args[1])
    return launch(args)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
