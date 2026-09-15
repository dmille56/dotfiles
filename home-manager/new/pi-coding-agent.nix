{
  pkgs,
  config,
  lib,
  ...
}:
let
  constants = import ./common-constants.nix;
  piNpmPrefix = "${config.home.homeDirectory}/.local/share/npm-global";
  piAgentNpmPrefix = "${config.home.homeDirectory}/.pi/agent/npm/node_modules";
  npmUserConfig = "${config.xdg.configHome}/npm/npmrc";

  piPackages = [
    "npm:@mariozechner/pi-tui"
    "npm:@ifi/pi-plan"
    "npm:@open-plan-annotator/pi-extension"
    "npm:pi-permission-system"
    "npm:pi-aliases"
    "npm:pi-generate-commit-message"
    "npm:pi-tool-display"
    "npm:pi-web-access"
    "npm:@juicesharp/rpiv-ask-user-question"
    "npm:@juicesharp/rpiv-todo"
    "npm:pi-rtk-optimizer"
    "git:github.com/cgxeiji/pi-emote"
    "git:github.com/dmille56/openvibes"
    "git:github.com/dmille56/pi-piper-tts"
  ];

  piWrapped = pkgs.writeShellApplication {
    name = "pi";
    text = ''
      export npm_config_prefix="${piNpmPrefix}"
      export npm_config_userconfig="${npmUserConfig}"

      open_plan_extension="${piAgentNpmPrefix}/@open-plan-annotator/pi-extension"
      open_plan_shared="${piAgentNpmPrefix}/open-plan-annotator/shared"
      open_plan_typebox="${piAgentNpmPrefix}/typebox"
      if [ -f "$open_plan_extension/extensions/index.js" ] && [ -d "$open_plan_shared" ] && [ -d "$open_plan_typebox" ]; then
        mkdir -p "$open_plan_extension/shared" "$open_plan_extension/node_modules/typebox"
        cp -f "$open_plan_shared"/*.mjs "$open_plan_extension/shared/"
        cp -a "$open_plan_typebox/." "$open_plan_extension/node_modules/typebox/"
        ${pkgs.perl}/bin/perl -0pi -e 's|from "typebox";|from "../node_modules/typebox/build/index.mjs";|' "$open_plan_extension/shared/piExtension.mjs"
        ${pkgs.perl}/bin/perl -0pi -e 's|return await import\("\.\./\.\./\.\./shared/piExtension\.mjs"\);|return await import("../shared/piExtension.mjs");|' "$open_plan_extension/extensions/index.js"
      fi

      exec ${pkgs.llm-agents.pi}/bin/pi "$@"
    '';
  };

  piInstallPackages = pkgs.writeShellApplication {
    name = "pi-install-packages";
    text = ''
      set -eu
      mkdir -p "${piNpmPrefix}"

      for pkg in ${lib.concatStringsSep " " piPackages}; do
        case "$pkg" in
          npm:*) pkg_name="''${pkg#npm:}" ;;
          *) pkg_name="$pkg" ;;
        esac

        if [ ! -d "${piNpmPrefix}/lib/node_modules/$pkg_name" ]; then
          PATH="${pkgs.git}/bin:${pkgs.nodejs}/bin:$PATH" \
            npm_config_prefix="${piNpmPrefix}" \
            npm_config_userconfig="${npmUserConfig}" \
            ${piWrapped}/bin/pi install "$pkg"

          if [ "$pkg_name" = "pi-web-access" ]; then
            pi_web_access_index="${piNpmPrefix}/lib/node_modules/pi-web-access/index.ts"
            if [ -f "$pi_web_access_index" ] && grep -q '@earendil-works/pi-ai/compat' "$pi_web_access_index"; then
              perl -0pi -e 's|from "@earendil-works/pi-ai/compat"|from "@earendil-works/pi-ai/base"|g' "$pi_web_access_index"
            fi
          fi
        fi
      done
    '';
  };

  testPiEmotesRepo = builtins.fetchGit {
    url = "https://github.com/dmille56/test-pi-emotes";
    rev = "900cf8e4a32e23247576fe8627da18bbaa5ba28f";
  };
in
with constants;
{
  home.packages = [
    piWrapped
    piInstallPackages
  ];

  programs.npm.enable = true;
  home.sessionPath = [ "${piNpmPrefix}/bin" ];
  home.sessionVariables = {
    npm_config_prefix = lib.mkDefault piNpmPrefix;
    npm_config_userconfig = lib.mkDefault npmUserConfig;
    PI_PERMISSION_SYSTEM_CONFIG_PATH = lib.mkDefault "${config.home.homeDirectory}/.pi/agent/extensions/pi-permission-system/config.json";
    PI_PERMISSION_SYSTEM_LOGS_DIR = lib.mkDefault "${config.home.homeDirectory}/.pi/agent/extensions/pi-permission-system/logs";
  };

  home.activation.ensureNpmUserConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -eu
    umask 077
    mkdir -p "${config.xdg.configHome}/npm"
    if [ ! -e "${npmUserConfig}" ]; then
      : > "${npmUserConfig}"
    fi
  '';

  home.file.".npmrc".source = config.lib.file.mkOutOfStoreSymlink npmUserConfig;
  home.file.".pi/agent/themes/dracula.json".text = lib.mkDefault (
    builtins.readFile (
      builtins.fetchurl {
        url = "https://raw.githubusercontent.com/dracula/pi-coding-agent/refs/heads/main/dracula.json";
        sha256 = "sha256:0whkxzj0rn4abj0dqvzhnykz69f7a0xmlswdxxrfiraddwnm8a34";
      }
    )
  );

  home.file.".pi/agent/AGENTS.md".text = ''
    When modifying files, always use the built-in read/write/edit tools. Avoid bash for file modifications and do not use sed/awk/nl | sed pipelines for editing/reading.
  '';

  home.file.".pi/agent/pi-permissions.jsonc".text = builtins.toJSON {
    defaultPolicy = {
      tools = "ask";
      bash = "ask";
      mcp = "ask";
      skills = "ask";
      special = "ask";
    };
    tools = {
      read = "allow";
      ls = "allow";
      grep = "allow";
      find = "allow";
      write = "allow";
      edit = "allow";
      set_plan = "allow";
      request_user_input = "allow";
      steer_task_agent = "allow";
      task_agents = "allow";
      annotate_plan = "ask";
      web_search = "allow";
      fetch_content = "allow";
      get_search_content = "allow";
      code_search = "allow";
      ask_user = "allow";
      ask_user_question = "allow";
      todo = "allow";
    };
    bash = {
      "git *" = "ask";
      "git status" = "allow";
      "git diff" = "allow";
      "git diff --stat" = "allow";
      "git log" = "allow";
      "git log --oneline" = "allow";
      "ls" = "allow";
      "npm *" = "ask";
      "npm test" = "allow";
      "npm run lint" = "allow";
      "npm run lint-fix" = "allow";
      "npm run build" = "allow";
      "npm run typecheck" = "allow";
      "npm run test" = "allow";
      "npm run format" = "allow";
      "ruff check" = "allow";
      "ruff check --fix" = "allow";
      "ruff format" = "allow";
      "mypy ." = "allow";
      "jobspy search *" = "allow";
      "*|*" = "ask";
      "*&&*" = "ask";
      "*||*" = "ask";
      "*;*" = "ask";
      "*&*" = "ask";
      "su" = "deny";
      "sudo *" = "deny";
      "nixos-rebuild *" = "deny";
      "home-manager *" = "deny";
    };
    skills = {
      jobspy = "allow";
      caveman = "allow";
      ask-user = "allow";
    };
    special = {
      external_directory = "ask";
      "external_directory:${constants.my-home-dir}/.agents/skills/*" = "allow";
    };
  };

  home.file.".pi/agent/extensions/pi-permission-system/config.json".text = builtins.toJSON {
    debug = true;
    yoloMode = false;
  };
  home.file.".pi/agent/extensions/pi-emote/emotes/cyber-greymane".source =
    "${testPiEmotesRepo}/emotes/cyber-greymane";
  home.file.".pi/agent/extensions/pi-emote/config.json".text = builtins.toJSON {
    terminals = [
      {
        match = "tmux";
        render = "auto";
      }
    ];
    emotes = [
      {
        model = "*";
        emote-set = "cyber-greymane";
      }
    ];
  };
  home.file.".pi/web-search.json".text = builtins.toJSON {
    provider = "exa";
    workflow = "none";
    searchModel = "openai/gpt-5.6-luna";
    summaryModel = "openai/gpt-5.6-luna";
  };

  systemd.user.services.pi-install-packages = {
    Unit.Description = "Install pi packages";
    Service = {
      Type = "oneshot";
      TimeoutStartSec = "60s";
      ExecStart = "${piInstallPackages}/bin/pi-install-packages";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
