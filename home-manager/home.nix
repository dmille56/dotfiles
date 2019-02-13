{ pkgs, ... }:

let
  my-dotfile-dir = "/home/dono/dotfiles";
in
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

    nix-prefetch-git

    #graphical

    #kdeconnect

    firefox xterm gparted
    chromium
    google-chrome

    mplayer alsaLib
    mpv
    vlc
    pavucontrol
    pasystray

    rofi dmenu conky

    xscreensaver feh

    gnome3.gedit
    gnome3.gnome-system-monitor
    gnome3.meld
    xfce.thunar

    streamlink

    dolphinEmu
    mupen64plus
    keepassx

    # redshift
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true; #remove this line if not compile
    initExtra = "if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi";

    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "git" ];
    };
  };

  programs.home-manager = {
    enable = true;
  };

  home.file.".tmux.conf".source = "${my-dotfile-dir}/tmux.conf";
  home.file.".vimrc".source = "${my-dotfile-dir}/vimrc";
  home.file.".Xresources".source = "${my-dotfile-dir}/Xresources";
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  home.file.".xmonad/xmonad.hs".source = "${my-dotfile-dir}/.xmonad/xmonad.hs";
}
