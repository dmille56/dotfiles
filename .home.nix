{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  home.packages = with pkgs; [
    wget vim curl git emacs firefox zsh networkmanager xterm gparted

    powershell
    tmux
    ranger
    fzf
    ripgrep
    tig
    lazygit

    bat exa fd

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
    urlview

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
