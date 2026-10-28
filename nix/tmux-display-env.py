"""Route new X11 launches to the latest tmux client or the local desktop."""

import argparse
import fcntl
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile


VARIABLES = ("DISPLAY", "XAUTHORITY", "WAYLAND_DISPLAY")


def tmux(*args):
    return subprocess.check_output(
        ["tmux", *socket_args, *args], text=True, stderr=subprocess.DEVNULL
    )


def local_file():
    return Path(os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")) / "tmux-local-display.json"


def save_local():
    if not os.environ.get("DISPLAY") or os.environ.get("SSH_CONNECTION") or os.environ.get("TMUX"):
        raise ValueError("Run save-local in a local graphical terminal outside tmux and SSH")
    destination = local_file()
    # Atomic replacement keeps concurrent hooks from reading a partial snapshot.
    with tempfile.NamedTemporaryFile(mode="w", dir=destination.parent, delete=False) as snapshot:
        json.dump({key: os.environ[key] for key in VARIABLES if key in os.environ}, snapshot)
    os.replace(snapshot.name, destination)


def local_environment():
    try:
        return json.loads(local_file().read_text())
    except FileNotFoundError:
        # Never fall back to the tmux server's initial environment: it might
        # belong to an old SSH connection. A local login saves the right values.
        return {}


def client_environment(pid):
    entries = Path(f"/proc/{pid}/environ").read_bytes().split(b"\0")
    return dict(os.fsdecode(entry).split("=", 1) for entry in entries if b"=" in entry)


def attached_clients():
    clients = {}
    for row in tmux("list-clients", "-F", "#{session_id} #{client_created} #{client_pid}").splitlines():
        session, created, pid = row.split()
        clients.setdefault(session, []).append((int(created), int(pid)))
    return clients


def choose_environment(clients):
    for _, pid in sorted(clients, reverse=True):
        try:
            return client_environment(pid)
        except FileNotFoundError:
            # A client can exit between list-clients and reading /proc.
            continue
    return local_environment()


def session_environment(session, clients):
    override = tmux("show-options", "-qv", "-t", session, "@display-env-here").strip()
    if override:
        saved = json.loads(override)
        if saved["clients"] == sorted([list(client) for client in clients]):
            return saved["environment"]
        # Connection changes resume automatic routing and discard stale pins.
        tmux("set-option", "-u", "-t", session, "@display-env-here")
    return choose_environment(clients)


def here():
    if os.environ.get("TMUX") and not args.client_pid:
        raise ValueError("Inside tmux, press prefix + X to select the invoking client's display")
    target = args.session or os.environ.get("TMUX_PANE")
    if not target:
        raise ValueError("Specify the tmux session: tmux-display-env here SESSION")
    session = tmux("display-message", "-p", "-t", target, "#{session_id}").strip()
    environment = client_environment(args.client_pid) if args.client_pid else os.environ
    if not environment.get("DISPLAY"):
        raise ValueError("This terminal has no DISPLAY; use a graphical terminal or ssh -X")
    clients = attached_clients().get(session, [])
    override = {"clients": sorted(clients), "environment": {
        key: environment[key] for key in VARIABLES if key in environment
    }}
    tmux("set-option", "-t", session, "@display-env-here", json.dumps(override))
    sync()
    print(f"New windows in {session} will use {environment['DISPLAY']} until a client attaches, detaches, or changes sessions")


def sync():
    # Hooks can overlap during rapid reconnects. Serialize their snapshots and
    # writes so an older hook cannot overwrite a newer client's environment.
    with (local_file().parent / "tmux-display-env.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        sync_sessions()


def sync_sessions():
    clients = attached_clients()
    commands = []
    for session in tmux("list-sessions", "-F", "#{session_id}").splitlines():
        environment = session_environment(session, clients.get(session, []))
        for key in VARIABLES:
            if commands:
                commands.append(";")
            if key in environment:
                commands.extend(["set-environment", "-t", session, key, environment[key]])
            else:
                commands.extend(["set-environment", "-r", "-t", session, key])
    if commands:
        tmux(*commands)


def shell():
    if not os.environ.get("TMUX"):
        return
    session = tmux("display-message", "-p", "-t", os.environ["TMUX_PANE"], "#{session_id}").strip()
    # Resolve live clients here as well: the next command need not wait for an
    # asynchronous attach/detach hook to finish updating the session.
    environment = session_environment(session, attached_clients().get(session, []))
    for key in VARIABLES:
        print(f"export {key}={shlex.quote(environment[key])}" if key in environment else f"unset {key}")


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--socket", help="tmux server socket (used by hooks)")
parser.add_argument("action", choices=("save-local", "sync", "shell", "here"))
parser.add_argument("session", nargs="?", help="target session for here")
parser.add_argument("--client-pid", type=int, help="invoking tmux client (set by the key binding)")
args = parser.parse_args()
socket_args = ["-S", args.socket] if args.socket else []
try:
    {"save-local": save_local, "sync": sync, "shell": shell, "here": here}[args.action]()
except (OSError, ValueError, subprocess.CalledProcessError) as error:
    print(f"tmux-display-env: {error}", file=sys.stderr)
    sys.exit(1)
