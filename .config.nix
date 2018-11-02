{ config, pkgs, ... }:

{

  nixpkgs.config.pulseaudio = true;

  services.xserver = {
    enable = true;

    desktopManager.default = "none";
    desktopManager.xterm.enable = false;

    displayManager.lightdm.enable = true;

    windowManager.default = "xmonad";
    windowManager.xmonad.enable = true;
  };

  environment.systemPackages = with pkgs; [
    zsh
    powershell

    xterm
    tmux

    curl
    wget

    ranger
    fzf

    rofi
    dmenu

    emacs
    vim

    git

    stack

    mplayer
    mpv
    cmus

    firefox
    chromium

    powerline-fonts
  ];

  time.timeZone = "America/Los_Angeles";

}
