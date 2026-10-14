"""Real TUI/PTY regression tests. Set TMUX_BIN and TUI_BIN to run in an isolated server."""

import fcntl
import json
import os
from pathlib import Path
import pty
import select
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time
import unittest


@unittest.skipUnless(os.environ.get("TMUX_BIN") and os.environ.get("TUI_BIN"), "set TMUX_BIN and TUI_BIN")
class TmuxReviews(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="plannotator-tmux-test-")
        self.root = Path(self.temp.name)
        self.runtime = self.root / "runtime"
        self.runtime.mkdir()
        self.command = [os.environ["TMUX_BIN"], "-S", str(self.root / "tmux.sock")]
        self.environment = {**os.environ, "TMPDIR": str(self.runtime), "TERM": "xterm-256color",
                            "PLANNOTATOR_TUI_THEME": "dark"}
        self.environment.pop("TMUX", None)
        self.environment.pop("TMUX_PANE", None)
        self.clients = []
        self.reviews = []
        self.addCleanup(self.cleanup)
        self.first = self.tmux("-f", "/dev/null", "new-session", "-d", "-s", "reviews",
                               "-x", "160", "-y", "50", "-P", "-F", "#{pane_id}", "sleep 120").strip()
        self.tmux("set-option", "-g", "mouse", "on")
        self.attach("reviews")
        self.second = self.tmux("split-window", "-d", "-h", "-t", self.first,
                                "-P", "-F", "#{pane_id}", "sleep 120").strip()
        setting = self.root / "use-browser"
        setting.write_text("false\n")
        browser = self.root / "browser"
        browser.write_text(f"#!{sys.executable}\nprint('BROWSER_FALLBACK')\n")
        browser.chmod(0o755)
        tui = self.root / "tui"
        # Record the actual popup PTY size before replacing this process with the real TUI.
        tui.write_text(f"#!{sys.executable}\n" +
                       "import json, os, sys\nfrom pathlib import Path\n" +
                       "if sys.argv[1] != '--export':\n" +
                       "    size = os.get_terminal_size()\n" +
                       "    (Path(os.environ['PLANNOTATOR_DATA_DIR']).parent / 'size.json').write_text(" +
                       "json.dumps({'width': size.columns, 'height': size.lines}))\n" +
                       f"os.execv({os.environ['TUI_BIN']!r}, [{os.environ['TUI_BIN']!r}, *sys.argv[1:]])\n")
        tui.chmod(0o755)
        source = Path(__file__).with_name("plannotator-agent.py").read_text()
        for key, value in {"@BROWSER@": browser, "@TUI@": tui, "@TMUX@": self.command[0],
                           "@SETTING@": setting}.items():
            source = source.replace(key, str(value))
        self.launcher = self.root / "agent.py"
        self.launcher.write_text(source)

    def tmux(self, *args):
        return subprocess.check_output(self.command + list(args), env=self.environment, text=True)

    def attach(self, session):
        pid, terminal = pty.fork()
        if pid == 0:
            os.execve(self.command[0], self.command + ["attach-session", "-t", session], self.environment)
        fcntl.ioctl(terminal, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 160, 0, 0))
        self.clients.append(dict(pid=pid, terminal=terminal, screen=bytearray()))
        self.wait(lambda: any(line.endswith("\t" + str(pid)) for line in
                             self.tmux("list-clients", "-F", "#{client_name}\t#{client_pid}").splitlines()))
        return len(self.clients) - 1

    def drain(self):
        for client in self.clients:
            while select.select([client["terminal"]], [], [], 0)[0]:
                try:
                    data = os.read(client["terminal"], 65536)
                except OSError:
                    break
                if not data:
                    break
                client["screen"].extend(data)
                client["screen"] = client["screen"][-2_000_000:]

    def wait(self, condition, seconds=8):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            self.drain()
            if condition():
                return
            time.sleep(.03)
        self.fail("timed out waiting for the tmux review state")

    def start_review(self, pane, name):
        document = self.root / (name + ".md")
        document.write_text(f"# Review {name}\n\nShip Friday.\n")
        session = self.tmux("display-message", "-p", "-t", pane, "#{session_id}").strip()
        env = {**self.environment, "TMUX": f"{self.root / 'tmux.sock'},1,{session[1:]}", "TMUX_PANE": pane}
        process = subprocess.Popen([sys.executable, str(self.launcher), "annotate", str(document)],
                                   cwd=self.root, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        review = dict(process=process, document=document, pane=pane)
        self.reviews.append(review)
        return review

    def review_root(self, review):
        for request in self.runtime.glob("plannotator-review-*/request.json"):
            try:
                if json.loads(request.read_text())["target"] == str(review["document"]):
                    return request.parent
            except (OSError, ValueError):
                pass
        return None

    def entries(self):
        directory = self.runtime / f"plannotator-popup-{os.getuid()}"
        return [entry for state in directory.glob("*.json")
                for entry in json.loads(state.read_text())["entries"]]

    def wait_open(self, review, client=0):
        def opened():
            root = self.review_root(review)
            return (root is not None and (root / "size.json").exists()
                    and f"Review {review['document'].stem}".encode() in self.clients[client]["screen"])
        self.wait(opened)
        root = self.review_root(review)
        size = json.loads((root / "size.json").read_text())
        dimensions = self.tmux("display-message", "-p", "-t", review["pane"],
                               "#{pane_width}\t#{pane_height}").strip().split("\t")
        self.assertEqual(size, {"width": int(dimensions[0]) - 2, "height": int(dimensions[1]) - 2})

    def finish(self, review, note=None, client=0):
        self.wait_open(review, client)
        if note:
            os.write(self.clients[client]["terminal"], b"c")
            time.sleep(.15)
            os.write(self.clients[client]["terminal"], note.encode() + b"\r")
            time.sleep(.15)
        os.write(self.clients[client]["terminal"], b"q")
        self.wait(lambda: review["process"].poll() is not None)
        output, error = review["process"].communicate()
        self.assertEqual(review["process"].returncode, 0, error)
        self.assertNotIn("opening browser", error)
        return output

    def cleanup(self):
        for review in self.reviews:
            if review["process"].poll() is None:
                review["process"].terminate()
                try:
                    review["process"].communicate(timeout=3)
                except subprocess.TimeoutExpired:
                    review["process"].kill()
                    review["process"].communicate()
            else:
                review["process"].communicate()
        subprocess.run(self.command + ["kill-server"], env=self.environment, capture_output=True)
        for client in self.clients:
            os.close(client["terminal"])
            try:
                os.kill(client["pid"], signal.SIGTERM)
            except ProcessLookupError:
                pass
            os.waitpid(client["pid"], 0)
        self.temp.cleanup()

    def test_visible_panes_queue_and_receive_separate_feedback(self):
        first = self.start_review(self.first, "alpha")
        self.wait_open(first)
        second = self.start_review(self.second, "beta")
        self.wait(lambda: len(self.entries()) == 2)
        self.assertEqual(sum(entry["active"] for entry in self.entries()), 1)
        self.assertFalse((self.review_root(second) / "worker.json").exists())
        first_feedback = self.finish(first, "Alpha only.")
        second_feedback = self.finish(second, "Beta only.")
        self.assertIn("Alpha only.", first_feedback)
        self.assertNotIn("Beta only.", first_feedback)
        self.assertIn("Beta only.", second_feedback)
        self.assertNotIn("Alpha only.", second_feedback)
        self.assertEqual(self.entries(), [])

    def test_hidden_request_allows_visible_review_and_uses_resized_geometry(self):
        hidden = self.tmux("new-window", "-d", "-P", "-F", "#{pane_id}", "sleep 120").strip()
        window = self.tmux("display-message", "-p", "-t", hidden, "#{window_id}").strip()
        hidden_review = self.start_review(hidden, "hidden")
        self.wait(lambda: len(self.entries()) == 1)
        self.assertFalse(self.entries()[0]["active"])
        self.tmux("split-window", "-d", "-h", "-t", hidden, "sleep 120")
        self.tmux("resize-pane", "-t", hidden, "-x", "65")
        visible_review = self.start_review(self.first, "visible")
        self.assertEqual(self.finish(visible_review), "")
        self.assertNotEqual(self.tmux("display-message", "-p", "#{window_id}").strip(), window)
        self.assertIsNone(hidden_review["process"].poll())
        self.tmux("select-window", "-t", window)
        self.assertEqual(self.finish(hidden_review), "")

    def test_zoom_hides_pending_pane(self):
        self.tmux("select-pane", "-t", self.first)
        self.tmux("resize-pane", "-Z", "-t", self.first)
        hidden_review = self.start_review(self.second, "zoomhidden")
        self.wait(lambda: len(self.entries()) == 1)
        self.assertFalse(self.entries()[0]["active"])
        visible_review = self.start_review(self.first, "zoomvisible")
        self.assertEqual(self.finish(visible_review), "")
        self.assertIsNone(hidden_review["process"].poll())
        self.tmux("resize-pane", "-Z", "-t", self.first)
        self.assertEqual(self.finish(hidden_review), "")

    def test_separate_clients_review_concurrently(self):
        other = self.tmux("new-session", "-d", "-s", "other", "-P", "-F", "#{pane_id}", "sleep 120").strip()
        client = self.attach("other")
        first = self.start_review(self.first, "clientone")
        second = self.start_review(other, "clienttwo")
        self.wait_open(first)
        self.wait_open(second, client)
        self.assertEqual(sum(entry["active"] for entry in self.entries()), 2)
        self.assertEqual(self.finish(first), "")
        self.assertEqual(self.finish(second, client=client), "")

    def test_waiting_cancellation_and_closed_pane_do_not_block(self):
        first = self.start_review(self.first, "open")
        self.wait_open(first)
        waiting = self.start_review(self.second, "cancelled")
        self.wait(lambda: len(self.entries()) == 2)
        waiting["process"].terminate()
        self.wait(lambda: waiting["process"].poll() is not None)
        self.assertNotEqual(waiting["process"].returncode, 0)
        self.wait(lambda: len(self.entries()) == 1)
        closing = self.start_review(self.second, "closed")
        self.wait(lambda: len(self.entries()) == 2)
        self.tmux("kill-pane", "-t", self.second)
        self.wait(lambda: closing["process"].poll() is not None)
        output, error = closing["process"].communicate()
        self.assertIn("BROWSER_FALLBACK", output)
        self.assertIn("requesting tmux pane closed", error)
        # The already-open popup keeps its original size until it closes.
        os.write(self.clients[0]["terminal"], b"q")
        self.wait(lambda: first["process"].poll() is not None)
        self.assertEqual(first["process"].returncode, 0)
        self.assertEqual(self.entries(), [])

    def test_active_cancellation_and_detach_release_queue(self):
        first = self.start_review(self.first, "cancelactive")
        self.wait_open(first)
        first["process"].terminate()
        self.wait(lambda: first["process"].poll() is not None)
        self.assertNotEqual(first["process"].returncode, 0)
        self.wait(lambda: not self.entries())
        second = self.start_review(self.second, "detach")
        self.wait_open(second)
        client_name = self.tmux("list-clients", "-F", "#{client_name}").strip()
        self.tmux("detach-client", "-t", client_name)
        self.wait(lambda: second["process"].poll() is not None)
        output, error = second["process"].communicate()
        self.assertIn("BROWSER_FALLBACK", output)
        self.assertIn("opening browser", error)
        self.assertEqual(self.entries(), [])

    def test_top_and_bottom_status_bars_with_quoted_filename(self):
        for position in ("top", "bottom"):
            self.tmux("set-option", "-g", "status-position", position)
            review = self.start_review(self.second, "quoted ' $(touch unexpected) #(touch unexpected_format)-" + position)
            self.assertEqual(self.finish(review), "")
            self.assertFalse((self.root / "unexpected").exists())
            self.assertFalse((self.root / "unexpected_format").exists())


if __name__ == "__main__":
    unittest.main()
