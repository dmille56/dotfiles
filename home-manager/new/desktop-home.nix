{ pkgs, config, lib, ...}: 
let
  constants = import ./common-constants.nix; 
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
  ];
  
  # :NOTE: open claw setup
  programs.openclaw = {
    enable = true;
    config = {
      gateway = {
        mode = "local";
        # :NOTE: don't need to set this since OPENCLAW_GATEWAY_TOKEN is set
        # auth = {
        #   # token = "$(cat ${config.sops.secrets.OPENCLAW_GATEWAY_TOKEN.path})";
        #   token = "<gatewayToken>"; # or set OPENCLAW_GATEWAY_TOKEN
        # };
      };

      channels.telegram = {
        tokenFile = "/run/secrets/OPENCLAW_TELEGRAM_BOT_TOKEN"; # any file path works
        allowFrom = [ 7696196772 ]; # your Telegram user ID
      };
    };

    # Built-ins (tools + skills) shipped via nix-steipete-tools.
    # plugins = [
    #   { source = "github:openclaw/nix-steipete-tools?dir=tools/summarize"; }
    # ];
    
    excludeTools = [ "git" "ripgrep" "ffmpeg" ];
    
    instances.default = {
      enable = true;
      # package = pkgs.openclaw; # batteries-included #:NOTE: this doesn't really work right because of installed tool collisions
      # stateDir = "~/.openclaw";
      # workspaceDir = "~/.openclaw/workspace";
      # launchd.enable = true;
      plugins = [
        # Example plugin without config:
        # { source = "github:acme/hello-world"; }
        { source = "github:openclaw/nix-steipete-tools?dir=tools/summarize"; }
      ];
    };
  };
  
  home.file.".background-image".source = my-desktop-background-image;
  
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  
  home.sessionVariables = {
    OPENCLAW_GATEWAY_TOKEN = "$(cat /run/secrets/OPENCLAW_GATEWAY_TOKEN)";
    OPENCLAW_NIX_MODE = "1";
    MY_MACHINE_ID = "desktop";
  };

}
