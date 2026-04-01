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
    excludeTools = [ "ffmpeg" "git" "ripgrep" ]; # tools you already have
    config = {
      gateway = {
        mode = "local";
        # auth = {
        #   # token = "<gatewayToken>"; # or set OPENCLAW_GATEWAY_TOKEN env
        # };
      };
      channels.telegram = {
        tokenFile = "/run/secrets/OPENCLAW_TELEGRAM_BOT_TOKEN";
        allowFrom = [ 7696196772 ];
      };
    };
    
    # instances.default = {
    #   enable = true;
    #   # systemd.enable = true;
    #   plugins = [
    #     { source = "github:openclaw/nix-steipete-tools?dir=tools/summarize"; }
    #   ];
    # };
  };
  
  home.file.".background-image".source = my-desktop-background-image;
  
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  
  home.sessionVariables = {
    OPENCLAW_GATEWAY_TOKEN = "$(cat /run/secrets/OPENCLAW_GATEWAY_TOKEN)";
    OPENCLAW_NIX_MODE = "1";
    MY_MACHINE_ID = "desktop";
  };

}
