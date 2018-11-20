{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    wget vim curl git emacs firefox zsh networkmanager xterm gparted

    powershell
    tmux
    ranger
    fzf
    ripgrep
    tig

    chromium
    google-chrome

    stack

    mplayer
    mpv
    vlc
    cmus
    pavucontrol

    rofi dmenu conky

    xscreensaver feh
    pandoc

    gnome3.gnome-system-monitor
    xfce.thunar
    xclip

    powerline-fonts

    rtv ddgr w3m youtube-dl streamlink

    youtube-viewer

    dolphinEmu
    mupen64plus
  ];
}
