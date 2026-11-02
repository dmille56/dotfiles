"""Browser queue checks; PLANNOTATOR_BIN enables a real-server integration."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest
from urllib.request import Request, urlopen

ROOT = Path(__file__).parent
spec = importlib.util.spec_from_file_location("opener", ROOT / "plannotator-browser-open.py")
opener = importlib.util.module_from_spec(spec)
spec.loader.exec_module(opener)


class BrowserQueueTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="plannotator-queue-test-")
        self.root = Path(self.temp.name)
        self.processes = []
        self.addCleanup(self.cleanup)
        self.env = {**os.environ, "PLANNOTATOR_BROWSER": "none", "HOME": str(self.root)}
        self.env.pop("TMUX", None)
        self.env.pop("TMUX_PANE", None)
        self.fake = self.root / "fake-browser"
        self.fake.write_text(f"#!{sys.executable}\n" + '''
import json, os, sys, time
from pathlib import Path
if len(sys.argv) == 1:
    request = json.loads(sys.stdin.buffer.read())
    marker, release, feedback = (request[key] for key in ('marker', 'release', 'feedback'))
else:
    marker, release, feedback = sys.argv[2:]
assert os.environ['PLANNOTATOR_PORT'] == '29432'
Path(marker).write_text(str(os.getpid()))
if feedback == 'FAIL':
    sys.exit(12)
while not Path(release).exists():
    time.sleep(.02)
sys.stdout.write(feedback)
''')
        self.fake.chmod(0o755)
        self.launcher = self.generate_launcher(self.fake)

    def cleanup(self):
        for process in self.processes:
            if process.poll() is None:
                process.terminate()
            process.communicate(timeout=8)
        self.temp.cleanup()

    def generate_launcher(self, binary, resolver_command=None):
        resolver = self.root / "resolver.py"
        resolver.write_text("import json,sys\nevent=json.load(sys.stdin)\n"
                            "if event.get('resolver_error'): sys.exit(2)\n"
                            "print(event.get('plan', ''))\n")
        browser_opener = self.root / "browser-opener"
        browser_opener.write_text(f"#!{sys.executable}\nimport runpy,sys\n"
                                  f"sys.argv = [{str(ROOT / 'plannotator-browser-open.py')!r}, "
                                  f"'https://castle.tailb6562d.ts.net:8443', *sys.argv[1:]]\n"
                                  f"with open({str(self.root / 'opened-urls')!r}, 'a') as out: out.write(sys.argv[-1] + '\\n')\n"
                                  f"runpy.run_path({str(ROOT / 'plannotator-browser-open.py')!r}, run_name='__main__')\n")
        browser_opener.chmod(0o755)
        source = (ROOT / "plannotator-agent.py").read_text()
        for key, value in {
            "@BROWSER@": binary, "@BROWSER_PORT@": "29432",
            "@BROWSER_URL@": "https://castle.tailb6562d.ts.net:8443",
            "@BROWSER_OPENER@": browser_opener,
            "@BUN@": resolver_command[0] if resolver_command else sys.executable,
            "@RESOLVER@": resolver_command[1] if resolver_command else resolver,
            "@SETTING@": self.root / "config/use-browser",
        }.items():
            source = source.replace(key, str(value))
        launcher = self.root / "launcher.py"
        launcher.write_text(source)
        return launcher

    def wait_for(self, predicate):
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            if predicate():
                return
            time.sleep(.02)
        self.fail("timed out waiting for review state")

    def start(self, name, feedback=None):
        marker, release = self.root / (name + ".pid"), self.root / (name + ".release")
        process = subprocess.Popen([sys.executable, str(self.launcher), "annotate",
                                    str(marker), str(release), feedback or name],
                                   stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE, env=self.env)
        self.processes.append(process)
        return process, marker, release

    def assert_queued(self, process):
        self.assertIn(b"review queued", process.stderr.readline())

    def test_two_reviews_serialize_and_return_their_own_feedback(self):
        first, first_marker, first_release = self.start("first", 'first "feedback"\n')
        self.wait_for(first_marker.exists)
        second, second_marker, second_release = self.start("second", "second feedback")
        self.assert_queued(second)
        self.assertFalse(second_marker.exists())
        first_release.touch()
        stdout, _ = first.communicate(timeout=8)
        self.assertEqual(stdout, b'first "feedback"\n')
        self.wait_for(second_marker.exists)
        second_release.touch()
        stdout, _ = second.communicate(timeout=8)
        self.assertEqual(stdout, b"second feedback")
        self.assertEqual(first.returncode, 0)
        self.assertEqual(second.returncode, 0)

    def test_canceling_waiter_does_not_start_it_or_interrupt_active_review(self):
        first, first_marker, first_release = self.start("first")
        self.wait_for(first_marker.exists)
        second, second_marker, _ = self.start("second")
        self.assert_queued(second)
        second.terminate()
        second.communicate(timeout=8)
        self.assertFalse(second_marker.exists())
        self.assertIsNone(first.poll())
        first_release.touch()
        self.assertEqual(first.communicate(timeout=8)[0], b"first")

    def test_canceling_active_review_releases_next_waiter(self):
        first, first_marker, _ = self.start("first")
        self.wait_for(first_marker.exists)
        second, second_marker, second_release = self.start("second")
        self.assert_queued(second)
        first.terminate()
        first.communicate(timeout=8)
        with self.assertRaises(ProcessLookupError):
            os.kill(int(first_marker.read_text()), 0)
        self.wait_for(second_marker.exists)
        second_release.touch()
        self.assertEqual(second.communicate(timeout=8)[0], b"second")

    def test_failed_server_does_not_hold_queue(self):
        failed, _, _ = self.start("failed", "FAIL")
        failed.communicate(timeout=8)
        self.assertEqual(failed.returncode, 12)
        next_review, marker, release = self.start("next")
        self.wait_for(marker.exists)
        release.touch()
        self.assertEqual(next_review.communicate(timeout=8)[0], b"next")

    def test_server_keeps_lock_when_launcher_is_forcibly_killed(self):
        first, first_marker, first_release = self.start("first")
        self.wait_for(first_marker.exists)
        second, second_marker, second_release = self.start("second")
        self.assert_queued(second)
        first.kill()
        first.wait(timeout=5)
        self.assertFalse(second_marker.exists())
        first_release.touch()
        self.wait_for(second_marker.exists)
        second_release.touch()
        self.assertEqual(second.communicate(timeout=8)[0], b"second")

    def test_hook_input_and_json_feedback_are_preserved(self):
        marker, release = self.root / "hook.pid", self.root / "hook.release"
        feedback = '{"decision":"block","reason":"fix this"}\n'
        event = json.dumps(dict(hook_event_name="Stop", plan="# Review this plan",
                                marker=str(marker), release=str(release), feedback=feedback)).encode()
        release.touch()
        process = subprocess.Popen([sys.executable, str(self.launcher)], stdin=subprocess.PIPE,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=self.env)
        self.processes.append(process)
        stdout, stderr = process.communicate(event, timeout=8)
        self.assertEqual(process.returncode, 0, stderr)
        self.assertEqual(stdout, feedback.encode())

    def test_noop_stop_hooks_exit_while_another_review_is_active(self):
        active, marker, release = self.start("active")
        self.wait_for(marker.exists)
        for event in ({"hook_event_name": "Stop"},
                      {"hook_event_name": "Stop", "stop_hook_active": True}):
            with self.subTest(event=event):
                result = subprocess.run([sys.executable, str(self.launcher)],
                                        input=json.dumps(event).encode(), capture_output=True,
                                        env=self.env, timeout=3)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, b"")
                self.assertNotIn(b"queued", result.stderr)
                self.assertIsNone(active.poll())
        release.touch()
        self.assertEqual(active.communicate(timeout=8)[0], b"active")

    def test_eligible_stop_hook_still_queues_and_preserves_event(self):
        active, marker, release = self.start("active")
        self.wait_for(marker.exists)
        hook_marker, hook_release = self.root / "queued-hook.pid", self.root / "queued-hook.release"
        event = json.dumps(dict(hook_event_name="Stop", plan="# Revised plan", stop_hook_active=True,
                                marker=str(hook_marker), release=str(hook_release), feedback="hook-feedback")).encode()
        hook = subprocess.Popen([sys.executable, str(self.launcher)], stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=self.env)
        self.processes.append(hook)
        hook.stdin.write(event)
        hook.stdin.close()
        hook.stdin = None
        self.assert_queued(hook)
        self.assertFalse(hook_marker.exists())
        release.touch()
        active.communicate(timeout=8)
        self.wait_for(hook_marker.exists)
        hook_release.touch()
        stdout, stderr = hook.communicate(timeout=8)
        self.assertEqual(hook.returncode, 0, stderr)
        self.assertEqual(stdout, b"hook-feedback")

    def test_resolver_failure_exits_without_waiting_or_starting_a_server(self):
        active, marker, release = self.start("active")
        self.wait_for(marker.exists)
        result = subprocess.run([sys.executable, str(self.launcher)],
                                input=b'{"hook_event_name":"Stop","resolver_error":true}',
                                capture_output=True, env=self.env, timeout=3)
        self.assertEqual(result.returncode, 1)
        self.assertIn(b"Could not resolve", result.stderr)
        self.assertNotIn(b"queued", result.stderr)
        self.assertIsNone(active.poll())
        release.touch()
        active.communicate(timeout=8)

    @unittest.skipUnless(os.environ.get("BUN") and os.environ.get("RESOLVER"),
                         "set BUN and RESOLVER for upstream Stop eligibility")
    def test_upstream_noop_stop_hooks_bypass_an_active_review(self):
        self.generate_launcher(self.fake, (os.environ["BUN"], os.environ["RESOLVER"]))
        active, marker, release = self.start("active")
        self.wait_for(marker.exists)
        transcript = self.root / "rollout.jsonl"
        def message(text, role="assistant"):
            return {"type": "response_item", "payload": {"type": "message", "role": role,
                    "content": [{"type": "output_text" if role == "assistant" else "input_text", "text": text}]}}
        cases = [([message("Done, no plan to review.")], False),
                 ([message("<proposed_plan>same plan</proposed_plan>"),
                   message("<hook_prompt>Revise this</hook_prompt>", "user")], True)]
        for entries, stop_hook_active in cases:
            with self.subTest(stop_hook_active=stop_hook_active):
                transcript.write_text("\n".join(json.dumps(entry) for entry in [
                    {"type": "turn_context", "payload": {"turn_id": "current"}}, *entries]))
                event = {"hook_event_name": "Stop", "transcript_path": str(transcript),
                         "turn_id": "current", "stop_hook_active": stop_hook_active}
                result = subprocess.run([sys.executable, str(self.launcher)],
                                        input=json.dumps(event).encode(), capture_output=True,
                                        env=self.env, timeout=3)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, b"")
                self.assertNotIn(b"queued", result.stderr)
                self.assertIsNone(active.poll())
        release.touch()
        active.communicate(timeout=8)

    def test_fixed_https_url_keeps_path_query_and_fragment(self):
        self.assertEqual(opener.review_url("https://castle.tailb6562d.ts.net:8443",
                                          "http://localhost:19432/review?a=1#notes"),
                         "https://castle.tailb6562d.ts.net:8443/review?a=1#notes")

    @unittest.skipUnless(os.environ.get("PLANNOTATOR_BIN"), "set PLANNOTATOR_BIN for real-server integration")
    def test_real_reviews_use_one_port_sequentially(self):
        self.generate_launcher(os.environ["PLANNOTATOR_BIN"])
        doc = self.root / "plan.md"
        doc.write_text("# Isolated queued review\n\nReview this.\n")
        def start(name):
            ready = self.root / (name + ".jsonl")
            env = {**self.env, "PLANNOTATOR_READY_FILE": str(ready),
                   "PLANNOTATOR_DATA_DIR": str(self.root / "data"), "PLANNOTATOR_AI": "disabled",
                   "PLANNOTATOR_ALLOWED_HOSTS": "castle.tailb6562d.ts.net",
                   "PLANNOTATOR_BROWSER": "", "PLANNOTATOR_SKIP_BROWSER_OPEN": "0",
                   "DISPLAY": "", "WAYLAND_DISPLAY": ""}
            process = subprocess.Popen([sys.executable, str(self.launcher), "annotate", str(doc)],
                                       stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE, env=env, cwd=self.root)
            self.processes.append(process)
            return process, ready
        def submit(feedback):
            request = Request("http://127.0.0.1:29432/api/feedback",
                              data=json.dumps({"feedback": feedback, "annotations": []}).encode(),
                              headers={"Content-Type": "application/json",
                                       "Host": "castle.tailb6562d.ts.net:8443",
                                       "Origin": "https://castle.tailb6562d.ts.net:8443",
                                       "Sec-Fetch-Site": "same-origin"})
            with urlopen(request, timeout=5) as response:
                self.assertEqual(response.status, 200)
        first, first_ready = start("first")
        self.wait_for(lambda: first_ready.exists() or first.poll() is not None)
        self.assertIsNone(first.poll(), first.stderr.read() if first.poll() is not None else "")
        self.wait_for((self.root / "opened-urls").exists)
        second, second_ready = start("second")
        self.assert_queued(second)
        self.assertFalse(second_ready.exists())
        submit("first-real-feedback")
        stdout, stderr = first.communicate(timeout=10)
        self.assertEqual(first.returncode, 0, stderr)
        self.assertIn(b"first-real-feedback", stdout)
        self.assertIn(b"https://castle.tailb6562d.ts.net:8443", stderr)
        self.wait_for(second_ready.exists)
        self.wait_for(lambda: len((self.root / "opened-urls").read_text().splitlines()) == 2)
        submit("second-real-feedback")
        stdout, stderr = second.communicate(timeout=10)
        self.assertEqual(second.returncode, 0, stderr)
        self.assertIn(b"second-real-feedback", stdout)
        for ready in (first_ready, second_ready):
            self.assertEqual(json.loads(ready.read_text().splitlines()[0])["port"], 29432)


if __name__ == "__main__":
    unittest.main()
