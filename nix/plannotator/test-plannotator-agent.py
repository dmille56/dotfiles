"""Run with Python; optional BUN and RESOLVER enable upstream parser fixtures."""

import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("agent", Path(__file__).with_name("plannotator-agent.py"))
agent = importlib.util.module_from_spec(spec)
spec.loader.exec_module(agent)


class LauncherTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.setting = patch.object(agent, "SETTING", self.root / "config/use-browser")
        self.setting.start()
        self.addCleanup(self.setting.stop)
        self.addCleanup(self.temp.cleanup)

    def set_mode(self, value):
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(agent.mode([value]), 0)

    def test_default_invalid_and_global_switch_without_restart(self):
        self.assertTrue(agent.browser_mode())
        self.set_mode("tui")
        self.assertFalse(agent.browser_mode())
        self.assertEqual(agent.SETTING.read_text(), "false\n")
        # A second process sees the setting, independent of inherited environment.
        code = (f"import runpy; m = runpy.run_path({str(Path(agent.__file__))!r}); "
                f"m['browser_mode'].__globals__['SETTING'] = m['Path']({str(agent.SETTING)!r}); "
                "print(m['browser_mode']())")
        self.assertEqual(subprocess.check_output([os.sys.executable, "-c", code], text=True).strip(), "False")
        self.set_mode("browser")
        self.assertTrue(agent.browser_mode())
        agent.SETTING.write_text("garbage")
        self.assertTrue(agent.browser_mode())
        agent.SETTING.write_bytes(b"\xff")
        self.assertTrue(agent.browser_mode())
        self.assertEqual(agent.mode(["invalid"]), 2)

    def test_client_selection_respects_server_window_and_session(self):
        def fake_run(command, **kwargs):
            self.assertEqual(command[:3], [agent.TMUX, "-S", "/tmp/my,socket"])
            output = ("@12\n" if "display-message" in command else
                      "other-window\t$4\t@99\t999\n"
                      "linked-window\t$5\t@12\t999\n"
                      "agent-client\t$4\t@12\t100\n")
            return subprocess.CompletedProcess(command, 0, stdout=output)
        with patch.dict(os.environ, {"TMUX": "/tmp/my,socket,123,4", "TMUX_PANE": "%7"}, clear=True), \
                patch.object(agent, "run", side_effect=fake_run):
            self.assertEqual(agent.tmux_context()[1], "agent-client")

    def test_no_tmux_stale_socket_and_no_visible_client(self):
        with patch.dict(os.environ, {}, clear=True):
            self.assertIsNone(agent.tmux_context())
        with patch.dict(os.environ, {"TMUX": "/tmp/socket,123,4", "TMUX_PANE": "%7"}, clear=True):
            with patch.object(agent, "run", side_effect=OSError("gone")):
                self.assertIsNone(agent.tmux_context())
            with patch.object(agent, "run", side_effect=[
                    subprocess.CompletedProcess([], 0, stdout="@2\n"),
                    subprocess.CompletedProcess([], 0, stdout="client\t$4\t@9\t1\n")]):
                self.assertIsNone(agent.tmux_context())

    def test_supported_targets_and_browser_fallbacks(self):
        target = self.root / "quoted ' $(touch nope).md"
        target.write_text("# Plan")
        self.assertEqual(agent.supported_target(["annotate", str(target)]), target)
        self.assertEqual(agent.supported_target(["annotate", str(self.root)]), self.root)
        for args in (["review"], ["review", "https://example/pr/1"],
                     ["annotate", "https://example/plan.md"],
                     ["annotate", str(target), "--render-html"], ["annotate", "missing.md"]):
            self.assertIsNone(agent.supported_target(args))
            self.set_mode("tui")
            with patch.object(agent, "tmux_context", return_value=(["tmux"], "client")), \
                    patch.object(agent, "browser", return_value=12) as browser:
                self.assertEqual(agent.launch(args), 12)
                browser.assert_called_once_with(args, None)

    def test_browser_default_and_outside_tmux(self):
        for use_tui in (False, True):
            if use_tui:
                self.set_mode("tui")
            with patch.object(agent, "tmux_context", return_value=None), \
                    patch.object(agent, "browser", return_value=0) as browser:
                self.assertEqual(agent.launch(["last"]), 0)
                browser.assert_called_once_with(["last"], None)

    def test_hook_approval_feedback_and_failure_preserve_input(self):
        self.set_mode("tui")
        event = b'{"hook_event_name":"Stop","turn_id":"current"}'
        for feedback in ("", "## Annotation 1\n> Change this"):
            stdin = io.TextIOWrapper(io.BytesIO(event))
            output = io.StringIO()
            directories = []
            def popup(context, root, target):
                directories.append(root)
                self.assertEqual(target.read_text(), "# My plan")
                return feedback
            with patch.object(agent.sys, "stdin", stdin), \
                    patch.object(agent, "tmux_context", return_value=(["tmux"], "client")), \
                    patch.object(agent, "run", return_value=subprocess.CompletedProcess([], 0, stdout=b"# My plan")), \
                    patch.object(agent, "popup", side_effect=popup), \
                    contextlib.redirect_stdout(output):
                self.assertEqual(agent.launch([]), 0)
            self.assertFalse(directories[0].exists())
            self.assertEqual(json.loads(output.getvalue()),
                             {"decision": "block", "reason": feedback} if feedback else {})
        with patch.object(agent.sys, "stdin", io.TextIOWrapper(io.BytesIO(event))), \
                patch.object(agent, "tmux_context", return_value=(["tmux"], "client")), \
                patch.object(agent, "run", return_value=subprocess.CompletedProcess([], 0, stdout=b"# My plan")), \
                patch.object(agent, "popup", side_effect=FileNotFoundError("dismissed")), \
                patch.object(agent, "browser", return_value=0) as browser:
            self.assertEqual(agent.launch([]), 0)
            browser.assert_called_once_with([], event)

    def test_no_plan_does_not_open_a_review(self):
        self.set_mode("tui")
        with patch.object(agent.sys, "stdin", io.TextIOWrapper(io.BytesIO(b"{}"))), \
                patch.object(agent, "tmux_context", return_value=(["tmux"], "client")), \
                patch.object(agent, "run", return_value=subprocess.CompletedProcess([], 0, stdout=b"")), \
                patch.object(agent, "popup") as popup:
            self.assertEqual(agent.launch([]), 0)
            popup.assert_not_called()

    def test_last_is_resolved_before_popup_and_failure_falls_back(self):
        self.set_mode("tui")
        order = []
        def resolve(command, **kwargs):
            order.append("resolve")
            self.assertEqual(command, [agent.TUI, "last", "--host", "codex", "--print"])
            return subprocess.CompletedProcess([], 0, stdout="# Latest response")
        def popup(context, root, target):
            order.append("popup")
            self.assertEqual(target.read_text(), "# Latest response")
            return "Feedback"
        with patch.object(agent, "tmux_context", return_value=(["tmux"], "client")), \
                patch.object(agent, "run", side_effect=resolve), \
                patch.object(agent, "popup", side_effect=popup), \
                contextlib.redirect_stdout(io.StringIO()) as output:
            self.assertEqual(agent.launch(["last"]), 0)
            self.assertEqual(output.getvalue(), "Feedback\n")
        self.assertEqual(order, ["resolve", "popup"])
        with patch.object(agent, "tmux_context", return_value=(["tmux"], "client")), \
                patch.object(agent, "run", return_value=subprocess.CompletedProcess([], 0, stdout="")), \
                patch.object(agent, "browser", return_value=0) as browser:
            self.assertEqual(agent.launch(["last"]), 0)
            browser.assert_called_once_with(["last"], None)

    def test_worker_exports_only_after_success_with_isolated_state(self):
        request = {"target": str(self.root / "doc.md"),
                   "environment": {"CODEX_THREAD_ID": "caller", "CODEX_HOME": None}}
        (self.root / "request.json").write_text(json.dumps(request))
        def fake_run(command, **kwargs):
            self.assertEqual(kwargs["env"]["CODEX_THREAD_ID"], "caller")
            self.assertNotIn("CODEX_HOME", kwargs["env"])
            self.assertNotIn("HERDR_ENV", kwargs["env"])
            self.assertEqual(kwargs["env"]["PLANNOTATOR_DATA_DIR"], str(self.root / "data"))
            return subprocess.CompletedProcess([], 0, stdout="No annotations.")
        with patch.dict(os.environ, {"HERDR_ENV": "1", "CODEX_HOME": "/server"}), \
                patch.object(agent, "run", side_effect=fake_run):
            self.assertEqual(agent.popup_worker(self.root), 0)
        self.assertEqual(json.loads((self.root / "completed.json").read_text()), {"feedback": ""})
        (self.root / "completed.json").unlink()
        with patch.object(agent, "run", side_effect=subprocess.CalledProcessError(1, ["tui"])):
            with self.assertRaises(subprocess.CalledProcessError):
                agent.popup_worker(self.root)
        self.assertFalse((self.root / "completed.json").exists())

    def test_popup_quotes_paths_and_requires_completion_marker(self):
        import shlex
        def fake_run(command, **kwargs):
            self.assertEqual(command[command.index("-c") + 1], "agent-client")
            self.assertEqual(shlex.split(command[-1])[-1], str(self.root))
            return subprocess.CompletedProcess([], 0)
        with patch.object(agent, "run", side_effect=fake_run):
            with self.assertRaises(FileNotFoundError):
                agent.popup((["tmux", "-S", "sock"], "agent-client"), self.root, self.root / "a 'b.md")


@unittest.skipUnless(os.environ.get("BUN") and os.environ.get("RESOLVER"), "set BUN and RESOLVER for upstream fixtures")
class ResolverTests(unittest.TestCase):
    def resolve(self, entries, **event):
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "rollout.jsonl"
            transcript.write_text("\n".join(json.dumps(entry) for entry in entries))
            payload = {"hook_event_name": "Stop", "transcript_path": str(transcript), **event}
            return subprocess.check_output([os.environ["BUN"], os.environ["RESOLVER"]],
                                           input=json.dumps(payload), text=True)

    @staticmethod
    def turn(turn_id):
        return {"type": "turn_context", "payload": {"turn_id": turn_id}}

    @staticmethod
    def message(text, role="assistant"):
        return {"type": "response_item", "payload": {"type": "message", "role": role,
                "content": [{"type": "output_text" if role == "assistant" else "input_text", "text": text}]}}

    def test_current_turn_and_ordinary_response(self):
        entries = [self.turn("old"), self.message("<proposed_plan>old</proposed_plan>"),
                   self.turn("new"), self.message("Done")]
        self.assertEqual(self.resolve(entries, turn_id="new"), "")
        entries.append(self.message("<proposed_plan>new</proposed_plan>"))
        self.assertEqual(self.resolve(entries, turn_id="new"), "new")
        self.assertEqual(self.resolve(entries, turn_id="absent"), "")
        self.assertEqual(self.resolve(entries, turn_id=""), "")
        self.assertEqual(self.resolve(entries), "new")

    def test_unchanged_and_revised_plan_after_feedback(self):
        entries = [self.turn("t"), self.message("<proposed_plan>plan</proposed_plan>"),
                   self.message("<hook_prompt>Change this</hook_prompt>", role="user")]
        self.assertEqual(self.resolve(entries, turn_id="t", stop_hook_active=True), "")
        entries.append(self.message("<proposed_plan>plan</proposed_plan>"))
        self.assertEqual(self.resolve(entries, turn_id="t", stop_hook_active=True), "")
        entries.append(self.message("<proposed_plan>revised</proposed_plan>"))
        self.assertEqual(self.resolve(entries, turn_id="t", stop_hook_active=True), "revised")

    def test_completed_plan_item_preferred_over_assistant_text(self):
        entries = [self.turn("t"), {"type": "event_msg", "payload": {
            "type": "item_completed", "turn_id": "t", "item": {"type": "plan", "text": "persisted"}}},
            self.message("<proposed_plan>assistant</proposed_plan>")]
        self.assertEqual(self.resolve(entries, turn_id="t"), "persisted")


if __name__ == "__main__":
    unittest.main()
