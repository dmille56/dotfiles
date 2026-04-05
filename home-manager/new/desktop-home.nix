{ pkgs, config, lib, ...}: 
let
  constants = import ./common-constants.nix; 
  openclawNoTools = (pkgs.openclawPackages.withTools {
    excludeToolNames = [ "ffmpeg" "git" "ripgrep" ];
  }).openclaw;
in with constants;
{
  imports = [ ./common-home.nix ];

  home.packages = with pkgs; [
    sensible-utils # :NOTE: added
    xdg-utils # :NOTE: added
    libnotify # :NOTE: added
    wifitui # :NOTE: added
    zoom-us # :NOTE: added
    remmina # :NOTE: added
    xmessage # :NOTE: added
    x2goclient # :NOTE: added

    kdePackages.dolphin
    nautilus
    obs-studio

    ollama-cuda
    # openclawNoTools
    # openclaw-gateway
    
    nvtopPackages.nvidia
  ];
  
  # :NOTE: open claw setup
  programs.openclaw = {
    enable = true;
    excludeTools = [ "ffmpeg" "git" "ripgrep" ]; # tools you already have
    package = pkgs.openclaw-gateway; # :NOTE: use openclaw-gateway until they fix this issue: https://github.com/openclaw/nix-openclaw/pull/81
    # config = {
    #   gateway = {
    #     mode = "local";
    #     # auth = {
    #     #   # token = "<gatewayToken>"; # or set OPENCLAW_GATEWAY_TOKEN env
    #     # };
    #   };
    #   channels.telegram = {
    #     tokenFile = "/run/secrets/OPENCLAW_TELEGRAM_BOT_TOKEN";
    #     allowFrom = [ 7696196772 ];
    #   };
    # };
    
    instances.default = {
      enable = true;
      systemd.enable = true;
      config = {
        gateway = {
          mode = "local";
          auth = {
            token = "nJgh6CqBrdYX0Nxpv6nuxTYJxpO1XxlGdF/Uo2mJFMM="; # or set OPENCLAW_GATEWAY_TOKEN env
          };
        };
        channels.telegram = {
          enabled = true;
          tokenFile = "/run/secrets/OPENCLAW_TELEGRAM_BOT_TOKEN";
          dmPolicy = "allowlist";
          allowFrom = [ 7696196772 ];
        };
        models = {
          mode = "merge";
          providers = {
            ollama = {
              baseUrl = "http://localhost:11434/";
              api = "ollama";
              models = [
                {
                  id = "qwen3.5:latest";
                  name = "qwen3.5:latest";
                  contextWindow = 64000;
                }
              ];
            };
          };
        };
        agents.defaults = {
          model = "openai/gpt-5.4-mini";
        };
      };
      plugins = [
        { source = "github:openclaw/nix-steipete-tools/5f677a283da837cad26c1ce982d85ee181085fc6?dir=tools/summarize"; }
        { source = "github:openclaw/nix-steipete-tools/5f677a283da837cad26c1ce982d85ee181085fc6?dir=tools/gogcli"; }
        { source = "github:openclaw/nix-steipete-tools/5f677a283da837cad26c1ce982d85ee181085fc6?dir=tools/goplaces"; }
        # { source = "github:openclaw/nix-steipete-tools/5f677a283da837cad26c1ce982d85ee181085fc6?dir=tools/bird"; }
        { source = "github:dmille56/jobspy-plugin/c992c9d01d11d82ceff55f0d182c16801aab67be"; }
      ];
    };

    # customPlugins = [
    #   {
    #     source = "github:openclaw/nix-steipete-tools/5f677a283da837cad26c1ce982d85ee181085fc6?dir=tools/summarize";
    #   }
    # ];
  };
  
  # :NOTE: workaround for plugins - remove previously-copied skill files before HM writes symlinks
  home.activation.clearOpenclawSkillFiles = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
    for skill_dir in ~/.openclaw/workspace/skills/*/; do
      for skill_file in "$skill_dir"*; do
        if [ -f "$skill_file" ] && [ ! -L "$skill_file" ]; then
          $DRY_RUN_CMD rm -f "$skill_file"
        fi
      done
    done
  '';

  # :NOTE: workaround for plugins - convert symlinks to real files so plugins work correctly
  home.activation.fixOpenclawSkillSymlinks = lib.hm.dag.entryAfter ["writeBoundary"] ''
    for skill_dir in ~/.openclaw/workspace/skills/*/; do
      for skill_file in "$skill_dir"*; do
        if [ -L "$skill_file" ]; then
          target=$(readlink "$skill_file")
          $DRY_RUN_CMD cp --remove-destination "$target" "$skill_file"
        fi
      done
    done
  '';
  
  systemd.user.services."openclaw-gateway" = {
    Service = {
      EnvironmentFile = "/run/secrets/rendered/openclaw-gateway-env";
    };
  };
  
  home.file.".openclaw/openclaw.json".force = true;  # :NOTE: clobber existing openclaw config
  
  home.file.".background-image".source = my-desktop-background-image;
  
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  
  home.sessionVariables = {
    # OPENCLAW_GATEWAY_TOKEN = "$(cat /run/secrets/OPENCLAW_GATEWAY_TOKEN)";
    # OPENCLAW_TELEGRAM_BOT_TOKEN = "$(cat /run/secrets/OPENCLAW_TELEGRAM_BOT_TOKEN)";
    # TELEGRAM_BOT_TOKEN = "$(cat /run/secrets/OPENCLAW_TELEGRAM_BOT_TOKEN)";
    # OPENCLAW_CONTAINER = "openclaw";
    OPENCLAW_NIX_MODE = "1";
    MY_MACHINE_ID = "desktop";
  };

}
