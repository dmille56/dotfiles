{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  home.packages = with pkgs; [

    #terminal

    wget vim curl git emacs zsh networkmanager

    powershell
    tmux
    ranger
    fzf
    ripgrep
    tig
    lazygit

    stack

    bat exa fd

    cmus
    pandoc

    xclip
    powerline-fonts

    rtv ddgr w3m youtube-dl

    youtube-viewer
    urlview

    #graphical

    firefox xterm gparted
    chromium
    google-chrome

    stack

    mplayer
    mpv
    vlc
    pavucontrol

    rofi dmenu conky

    xscreensaver feh

    gnome3.gnome-system-monitor
    xfce.thunar

    streamlink

    dolphinEmu
    mupen64plus
    keepassx
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    initExtra = "if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi";

    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "git" ];
    };
  };
}
