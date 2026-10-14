"""Shared backend selection and tmux transport for Plannotator skills/hooks."""

import fcntl
import hashlib
import json
import os
from pathlib import Path
import shlex
import signal
import subprocess
import sys
import tempfile
import time
import uuid
from contextlib import contextmanager

BROWSER = "@BROWSER@"
TUI = "@TUI@"
TUICR = "@TUICR@"
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


def tmux_snapshot(command):
    clients = {}
    client_format = "\t".join("#{" + name + "}" for name in (
        "client_name", "session_id", "window_id", "client_activity", "client_pid",
        "client_width", "client_height", "status",
    ))
    for line in run(command + ["list-clients", "-F", client_format],
                    capture_output=True, text=True).stdout.splitlines():
        name, session, window, activity, pid, width, height, status = line.split("\t")
        status_lines = 0 if status == "off" else 1 if status == "on" else int(status)
        clients[name] = dict(session=session, window=window, activity=int(activity or 0),
                             pid=int(pid), width=int(width), height=int(height) - status_lines)
    panes = {}
    pane_format = "\t".join("#{" + name + "}" for name in (
        "pane_id", "window_id", "pane_left", "pane_top", "pane_width", "pane_height",
        "window_zoomed_flag", "pane_active", "pane_dead",
    ))
    for line in run(command + ["list-panes", "-a", "-F", pane_format],
                    capture_output=True, text=True).stdout.splitlines():
        pane, window, left, top, width, height, zoomed, active, dead = line.split("\t")
        panes[pane] = dict(window=window, left=int(left), top=int(top), width=int(width),
                           height=int(height), zoomed=zoomed == "1", active=active == "1", dead=dead == "1")
    return clients, panes


def tmux_context():
    """Bind to an attached client, including agents in its currently hidden windows."""
    context = os.environ.get("TMUX", "").rsplit(",", 2)
    pane = os.environ.get("TMUX_PANE", "")
    if len(context) != 3 or not context[0] or not pane:
        return None
    command = [TMUX, "-S", context[0]]
    try:
        clients, panes = tmux_snapshot(command)
        if pane not in panes or panes[pane]["dead"]:
            return None
        candidates = []
        for name, client in clients.items():
            same_session = client["session"] == "$" + context[2]
            visible = client["window"] == panes[pane]["window"]
            if same_session or visible:
                candidates.append((same_session, visible, client["activity"], name))
        if not candidates:
            return None
        name = max(candidates)[3]
        return dict(command=command, client=name, client_pid=clients[name]["pid"],
                    pane=pane, session="$" + context[2])
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        detail = (error.stderr or "").strip() if isinstance(error, subprocess.CalledProcessError) else str(error)
        raise OSError(
            f"Could not inspect tmux socket {context[0]}: {detail or error}. "
            "When launched by a coding agent, run the launcher outside the sandbox "
            "(exec_command: sandbox_permissions=\"require_escalated\")."
        ) from error


def visible(pane, client):
    return (not pane["dead"] and pane["window"] == client["window"]
            and (not pane["zoomed"] or pane["active"]))


def review_geometry(context, snapshot):
    clients, panes = snapshot
    client = clients.get(context["client"])
    pane = panes.get(context["pane"])
    if client is None or client["pid"] != context["client_pid"]:
        raise OSError("the review's tmux client disconnected")
    if pane is None or pane["dead"]:
        raise OSError("the requesting tmux pane closed")
    if not visible(pane, client):
        return None
    if (pane["width"] < 3 or pane["height"] < 3
            or pane["left"] + pane["width"] > client["width"]
            or pane["top"] + pane["height"] > client["height"]):
        raise OSError("the requesting pane cannot fit a popup in this client")
    return pane["width"], pane["height"]


def process_start(pid):
    """Identify living Linux processes without confusing a reused PID with its owner."""
    try:
        fields = Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()
        return None if fields[0] == "Z" else fields[19]
    except (OSError, IndexError):
        return None


def process_owner():
    return {"pid": os.getpid(), "start": process_start(os.getpid())}


def owner_alive(owner):
    return owner["start"] is not None and process_start(owner["pid"]) == owner["start"]


def worker_alive(root):
    try:
        return owner_alive(json.loads((Path(root) / "worker.json").read_text()))
    except (OSError, ValueError, KeyError):
        return False


class ReviewQueue:
    def __init__(self, context):
        self.context = context
        directory = Path(tempfile.gettempdir()) / f"plannotator-popup-{os.getuid()}"
        directory.mkdir(mode=0o700, exist_ok=True)
        key = hashlib.sha256(shlex.join(context["command"] + [context["client"]]).encode()).hexdigest()
        # Reuse the old lock name so an older launcher already reviewing can finish.
        self.lock = directory / key
        self.state = directory / (key + ".json")

    @contextmanager
    def locked(self):
        with self.lock.open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            try:
                state = json.loads(self.state.read_text())
            except FileNotFoundError:
                state = {"next": 0, "entries": []}
            # Keep an orphaned active popup reserved until its worker also exits.
            state["entries"] = [entry for entry in state["entries"]
                                if owner_alive(entry["owner"]) or
                                (entry["active"] and worker_alive(entry["root"]))]
            yield state
            with tempfile.NamedTemporaryFile(mode="w", dir=self.state.parent, delete=False) as out:
                temporary = Path(out.name)
                json.dump(state, out)
            try:
                os.replace(temporary, self.state)
            finally:
                temporary.unlink(missing_ok=True)

    def register(self, root):
        token = uuid.uuid4().hex
        with self.locked() as state:
            state["next"] += 1
            state["entries"].append(dict(token=token, order=state["next"],
                                         pane=self.context["pane"], root=str(root),
                                         owner=process_owner(), active=False))
        return token

    def claim(self, token, snapshot):
        clients, panes = snapshot
        client = clients.get(self.context["client"])
        with self.locked() as state:
            if any(entry["active"] for entry in state["entries"]):
                return False
            eligible = [entry for entry in state["entries"]
                        if entry["pane"] in panes and client is not None
                        and visible(panes[entry["pane"]], client)]
            if eligible and min(eligible, key=lambda entry: entry["order"])["token"] == token:
                next(entry for entry in state["entries"] if entry["token"] == token)["active"] = True
                return True
            return False

    def release(self, token, remove=False):
        with self.locked() as state:
            if remove:
                state["entries"] = [entry for entry in state["entries"] if entry["token"] != token]
            else:
                for entry in state["entries"]:
                    if entry["token"] == token:
                        entry["active"] = False


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
    (root / "worker.json").write_text(json.dumps(process_owner()))
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
    if request.get("review") is not None:
        feedback = run([TUICR, "--stdout", "--no-update-check", *request["review"]],
                       env=env, capture_output=True, text=True).stdout.strip()
    else:
        env["PLANNOTATOR_DATA_DIR"] = str(root / "data")
        run([TUI, request["target"]], env=env)
        feedback = run([TUI, "--export", request["target"]], env=env,
                       capture_output=True, text=True).stdout.strip()
        if feedback == "No annotations.":
            feedback = ""
    (root / "completed.json").write_text(json.dumps({"feedback": feedback}))
    return 0


def popup(context, root, target, review=None):
    command, client, pane = context["command"], context["client"], context["pane"]
    environment = {name: os.environ.get(name) for name in (
        "HOME", "PATH", "XDG_CONFIG_HOME", "CODEX_HOME", "CODEX_THREAD_ID",
        "PLANNOTATOR_TUI_CONFIG", "PLANNOTATOR_TUI_THEME",
    )}
    (root / "request.json").write_text(json.dumps({
        "target": str(target), "review": review, "environment": environment,
    }))
    worker = shlex.join([sys.executable, str(Path(__file__).resolve()),
                        "--popup-worker", str(root)])
    queue = ReviewQueue(context)
    token = queue.register(root)
    process = None
    active = False
    announced = False
    try:
        while True:
            snapshot = tmux_snapshot(command)
            geometry = review_geometry(context, snapshot)
            if geometry is not None and queue.claim(token, snapshot):
                active = True
                # The user may have resized or switched windows while we claimed.
                geometry = review_geometry(context, tmux_snapshot(command))
                if geometry is None:
                    queue.release(token)
                    active = False
                    continue
                width, height = geometry
                # tmux expands title formats; filenames must remain literal text.
                title = f"{'tuicr' if review is not None else 'Plannotator'} {pane}: {target.name}".replace("#", "##")
                process = subprocess.Popen(command + [
                    "display-popup", "-E", "-c", client, "-t", pane,
                    "-T", title,
                    "-w", str(width), "-h", str(height), "-x", "P", "-y", "P",
                    "-d", os.getcwd(), worker,
                ], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                while True:
                    try:
                        _, error = process.communicate(timeout=0.25)
                        break
                    except subprocess.TimeoutExpired:
                        # Detect disappearing targets even while the review is open.
                        review_geometry(context, tmux_snapshot(command))
                if process.returncode:
                    raise OSError(error.strip() or f"popup exited with status {process.returncode}")
                # A dismissed popup can return success without completing the worker.
                return json.loads((root / "completed.json").read_text())["feedback"]
            if not announced:
                print(f"Plannotator review queued for pane {pane}; it will open when its window is visible "
                      "and this client's current review has finished.", file=sys.stderr)
                announced = True
            time.sleep(0.25)
    finally:
        if active and process is not None and process.poll() is None:
            # Close only the popup whose worker belongs to this request.
            if worker_alive(root):
                subprocess.run(command + ["display-popup", "-C", "-c", client],
                               capture_output=True, check=False)
            process.terminate()
            try:
                process.communicate(timeout=2)
            except subprocess.TimeoutExpired:
                process.kill()
                process.communicate()
        queue.release(token, remove=True)


def browser(args, event):
    if event is None:
        os.execv(BROWSER, [BROWSER, *args])
    return subprocess.run([BROWSER, *args], input=event).returncode


def launch(args, force_tui=False):
    event = sys.stdin.buffer.read() if not args else None
    try:
        context = tmux_context() if force_tui or not browser_mode() else None
    except OSError as error:
        print(error, file=sys.stderr)
        return 1
    if context is None:
        if force_tui:
            print("tuicr-review requires an attached tmux client", file=sys.stderr)
            return 1
        return browser(args, event)
    review = None
    if args == ["review"]:
        review = ["-w"]
    elif len(args) == 2 and args[0] == "review" and args[1].startswith((
            "https://github.com/", "https://gitlab.com/")):
        review = ["pr", args[1]]
    target = supported_target(args)
    if review is not None:
        target = Path.cwd()
    elif args and args != ["last"] and target is None:
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
            if review is None:
                feedback = popup(context, root, target)
            else:
                feedback = popup(context, root, target, review=review)
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
    if args[:1] == ["--tuicr-review"]:
        return launch(["review", *args[1:]], force_tui=True)
    if args[:1] == ["--popup-worker"]:
        return popup_worker(args[1])
    return launch(args)


if __name__ == "__main__":
    # Tool-session cancellation must release queued requests and owned popups.
    def interrupted(_signum, _frame):
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, interrupted)
    signal.signal(signal.SIGHUP, interrupted)
    sys.exit(main(sys.argv[1:]))
