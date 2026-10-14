// Keep the same turn selection and feedback-loop guard as browser Plannotator.
import { existsSync } from "node:fs";
import {
  findCodexRolloutsByThreadId,
  logCodexStopSkip,
  logCodexStopTurnIdFallback,
  resolveCodexStopPlan,
} from "@CODEX_SESSION@";

const input = await Bun.stdin.text();
const event = JSON.parse(input);
if (event.hook_event_name !== "Stop") {
  throw new Error("unsupported hook event");
}
const candidates = typeof event.transcript_path === "string" && event.transcript_path
  ? [event.transcript_path]
  : process.env.CODEX_THREAD_ID
    ? findCodexRolloutsByThreadId(process.env.CODEX_THREAD_ID)
    : [];
const rollout = candidates.find((path: string) => existsSync(path));
if (rollout) {
  const rawTurnId = event.turn_id;
  const { plan, skipReason, fallbackTurnId } = resolveCodexStopPlan(rollout, {
    turnId: rawTurnId === undefined ? undefined : typeof rawTurnId === "string" ? rawTurnId : "",
    stopHookActive: !!event.stop_hook_active,
  });
  if (skipReason) logCodexStopSkip(skipReason, { debug: process.env.PLANNOTATOR_DEBUG });
  if (fallbackTurnId) logCodexStopTurnIdFallback(fallbackTurnId);
  if (plan?.text) process.stdout.write(plan.text);
}
