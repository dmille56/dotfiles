{ pkgs, config, lib, ... }:
let
  browser = pkgs.llm-agents.plannotator;
  tui = pkgs.callPackage ../../nix/plannotator/plannotator-tui.nix {};
  # Tested against the currently installed browser release. Fetch the standalone
  # parser rather than duplicating its current-turn and resubmission logic.
  codexSession = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/backnotprop/plannotator/v0.28.17/apps/hook/server/codex-session.ts";
    hash = "sha256-twlEOyh1X595AkjA9fNlmYBmOHmMp58peoBY0bholHc=";
  };
  resolver = pkgs.writeText "plannotator-plan.ts" (lib.replaceStrings
    [ "@CODEX_SESSION@" ] [ "${codexSession}" ]
    (builtins.readFile ../../nix/plannotator/plannotator-plan.ts));
  launcherSource = pkgs.writeText "plannotator-agent.py" (lib.replaceStrings
    [ "@BROWSER@" "@TUI@" "@TUICR@" "@TMUX@" "@BUN@" "@RESOLVER@" "@SETTING@" ]
    [ "${browser}/bin/plannotator" "${tui}/bin/plannotator-tui" "${pkgs.llm-agents.tuicr}/bin/tuicr"
      "${pkgs.tmux}/bin/tmux" "${pkgs.bun}/bin/bun" "${resolver}"
      "${config.home.homeDirectory}/.config/plannotator/use-browser" ]
    (builtins.readFile ../../nix/plannotator/plannotator-agent.py));
  launcher = pkgs.writeShellApplication {
    name = "plannotator-agent";
    runtimeInputs = [ pkgs.git ];
    text = ''exec ${pkgs.python3}/bin/python3 ${launcherSource} "$@"'';
    passthru = { inherit launcherSource resolver tui; };
  };
  mode = pkgs.writeShellApplication {
    name = "plannotator-mode";
    text = ''exec ${launcher}/bin/plannotator-agent --mode "$@"'';
  };
  rules = pkgs.writeText "plannotator.rules" ''
    prefix_rule(
      pattern = ["${launcher}/bin/plannotator-agent"],
      decision = "allow",
    )
    prefix_rule(
      pattern = ["${mode}/bin/plannotator-mode"],
      decision = "allow",
    )
  '';
  guidance = ''
    These skills are shared by Codex, OpenCode, and Pi. Run the launcher with the
    current agent's shell tool (`exec_command` in Codex, `bash` in OpenCode or Pi).
    In Codex, request `require_escalated` and explain that the launcher needs home
    writes and tmux or localhost socket access. The Home Manager allow rule permits
    the command but does not itself grant access outside the sandbox. In OpenCode
    and Pi, use their normal command approval flow; if their execution environment
    blocks home writes, tmux sockets, or localhost sockets, report the specific
    denial and stop. An approval prompt does not by itself remove a sandbox
    restriction. Never retry the same blocked command in the same sandbox or use
    sudo. If Codex does not recognize its allow rules after activation, confirm
    `~/.codex/rules/plannotator.rules` is a regular file and restart Codex.

    Plannotator writes review state under the user's home directory. Browser mode
    needs permission to listen on a localhost port; TUI and tuicr reviews need
    access to the existing tmux socket. Sandboxed execution can fail with EROFS
    for history writes or EPERM for sockets.

    The launcher reads the global user setting on each invocation. `plannotator-mode tui`
    selects tmux popup reviews; `plannotator-mode browser` selects browser reviews;
    `plannotator-mode status` shows the setting. The default is browser.
    Switching mode writes a global user setting outside the project; run the mode
    command outside the sandbox too. Both mode commands are covered by the allow rules.
    Outside tmux and for unsupported requests, it uses browser Plannotator.
    TUI popups cover the requesting agent's pane and queue one at a time per tmux
    client. Hidden panes wait until visible, including panes hidden by zoom.
    Return to the agent's window to review; the launcher does not switch windows.
    Keep the command running while queued. Other visible requests can proceed.
    In the TUI, leave annotations and quit with `q`; feedback is exported when the
    popup closes. Determine what artifact was reviewed and follow the matching
    loop below. Never stop after repeating or summarizing annotations.
  '';
  skills = {
    tuicr-review = pkgs.writeText "tuicr-review-SKILL.md" ''
      ---
      name: tuicr-review
      description: Review the current working tree or a GitHub/GitLab pull request with tuicr in a tmux popup. Use when asked for a code review.
      ---

      Run `${launcher}/bin/plannotator-agent --tuicr-review` to review the current
      working tree. Use the current agent's shell tool and approval mechanism as
      described below. This mode requires an attached tmux client and access to
      its socket on every invocation.
      Pass a GitHub or GitLab pull request URL as an argument to
      review that request. This command always opens tuicr in the requesting
      agent pane's tmux popup, regardless of the Plannotator browser/TUI setting.
      Keep it running until the popup closes, then use its returned review text
      to inspect and fix the relevant code findings. Then rerun this review on
      the updated working tree (or PR) and address any new actionable findings;
      repeat until the review has no actionable findings. Briefly report what
      changed and any findings left unresolved. If no attached tmux
      client is available, report that the review could not start.
      A tmux socket permission error means the execution environment blocked
      access; it does not prove that no tmux client is attached. Follow the
      environment-specific access guidance below; do not ask the user to start a
      second review manually.

      ${guidance}
    '';
    plannotator-review = pkgs.writeText "plannotator-review-SKILL.md" ''
      ---
      name: plannotator-review
      description: Review local changes or a GitHub/GitLab pull request with Plannotator. Use when asked to visually inspect or annotate code changes.
      ---

      Run `${launcher}/bin/plannotator-agent review` to review uncommitted changes.
      Pass a pull request or merge request URL as an argument when requested.
      In TUI mode, local changes and GitHub/GitLab PRs open in tuicr inside
      a tmux popup. Use `c` to comment, then `q` to finish; the review text is
      returned to the agent. Apply actionable review comments to the code and
      rerun this review on the updated working tree (or PR). Address any new
      actionable findings and repeat until none remain; do not merely repeat or
      summarize the comments. Outside tmux, reviews use browser Plannotator.

      ${guidance}
    '';
    plannotator-annotate = pkgs.writeText "plannotator-annotate-SKILL.md" ''
      ---
      name: plannotator-annotate
      description: Visually annotate a Markdown file, folder, URL, or HTML artifact with Plannotator.
      ---

      Run `${launcher}/bin/plannotator-agent annotate` followed by the requested file,
      folder, or URL. For rendered HTML, add `--render-html`. Markdown files and
      folders support the TUI; URLs, HTML, and other options use the browser.

      ${guidance}
    '';
    plannotator-last = pkgs.writeText "plannotator-last-SKILL.md" ''
      ---
      name: plannotator-last
      description: Open the most recent Codex response in Plannotator for visual annotation and feedback.
      ---

      Run `${launcher}/bin/plannotator-agent last` to review the latest Codex response.
      Treat the response as a plan when it proposes steps or an approach to carry
      out. Revise that plan to incorporate the annotations, present the revised
      plan to the user, then run this command again on the revised plan. Repeat
      until the plan is approved or no actionable feedback remains. Do not begin
      implementation while the artifact is still a plan. For feedback on a
      completed artifact, apply the requested changes and review the updated
      artifact again; repeat until no actionable feedback remains. If the
      artifact type is unclear, infer it from the surrounding task and context.

      ${guidance}
    '';
  };
in
{
  home.packages = [ browser tui launcher mode ];

  # Codex's startup discovery ignores symlinked .rules files. Let Home Manager
  # remove the old managed symlink, then install a regular file with current paths.
  # Keep default.rules mutable for approvals added by Codex itself.
  home.activation.installPlannotatorRules = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    run ${pkgs.coreutils}/bin/mkdir -p "${config.home.homeDirectory}/.codex/rules"
    run ${pkgs.coreutils}/bin/install -m 0644 "${rules}" "${config.home.homeDirectory}/.codex/rules/plannotator.rules"
  '';

  home.file.".codex/hooks.json".text = builtins.toJSON {
    hooks.Stop = [{
      hooks = [{
        type = "command";
        command = "${launcher}/bin/plannotator-agent";
        timeout = 345600;
      }];
    }];
  };

  # Codex skips symlinked SKILL.md files. Retain regular manifests and the
  # activation ordering used by previous generations.
  home.activation.installPlannotatorSkills = lib.hm.dag.entryAfter [ "linkGeneration" ]
    (lib.concatStringsSep "\n" (lib.mapAttrsToList (name: source: ''
      run ${pkgs.coreutils}/bin/mkdir -p "${config.home.homeDirectory}/.agents/skills/${name}"
      run ${pkgs.coreutils}/bin/install -m 0644 "${source}" "${config.home.homeDirectory}/.agents/skills/${name}/SKILL.md"
    '') skills));
}
