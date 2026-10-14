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
    [ "@BROWSER@" "@TUI@" "@TMUX@" "@BUN@" "@RESOLVER@" "@SETTING@" ]
    [ "${browser}/bin/plannotator" "${tui}/bin/plannotator-tui"
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
  guidance = ''
    Run the review command with the coding agent's approved execution outside its
    sandbox. Plannotator writes review state under the user's home directory and
    needs either access to the existing tmux socket or permission to listen on a
    localhost port for the browser UI. A normal sandboxed invocation can fail with
    EROFS for history writes or EPERM for sockets.
    With Codex's `exec_command`, set `sandbox_permissions` to `require_escalated`
    and provide a short `justification` explaining those requirements. When
    supported, scope a reusable `prefix_rule` to
    `["${launcher}/bin/plannotator-agent"]`. Other coding agents should use their
    equivalent execution approval mechanism. If approval is denied, report that
    the review could not start; do not retry inside the same sandbox. Do not use sudo.

    The launcher reads the global user setting on each invocation. `plannotator-mode tui`
    selects tmux popup reviews; `plannotator-mode browser` selects browser reviews;
    `plannotator-mode status` shows the setting. The default is browser.
    Switching mode writes a global user setting outside the project; use the
    same execution approval mechanism for `plannotator-mode tui` or `browser`.
    Outside tmux and for unsupported requests, it uses browser Plannotator.
    In the TUI, leave annotations and quit with `q`; feedback is exported when the
    popup closes. Wait for the command to finish and relay its feedback to the user.
  '';
  skills = {
    plannotator-review = pkgs.writeText "plannotator-review-SKILL.md" ''
      ---
      name: plannotator-review
      description: Review local changes or a GitHub/GitLab pull request with Plannotator. Use when asked to visually inspect or annotate code changes.
      ---

      Run `${launcher}/bin/plannotator-agent review` to review uncommitted changes.
      Pass a pull request or merge request URL as an argument when requested.
      Git and PR reviews use browser Plannotator.

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

      ${guidance}
    '';
  };
in
{
  home.packages = [ browser tui launcher mode ];

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
