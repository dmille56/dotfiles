{ pkgs, ... }:

# How to upgrade nix pkgs:
# 1. nix-channel --update
# 2. nix-env --upgrade
# 3. home-manager switch

# Upgrade ubuntu:
# 1. sudo apt-get update
# 2. sudo apt-get upgrade

# Garbage collect nix:
# nix-collect-garbage -d

# Theme help:
#   - install qt5ct & kvantum for KDE themes with ubuntu
#   - xfsettingsd #has to be run at startup for xfce theme setting to work
#   - export QT_QPA_PLATFORMTHEME=qt5ct # for KDE themes to work... in .profile

# Non nix:
# - install spotify
# - install steam
# - install nix
# - install kdeconnect
#
# add to .profile to fix locale issue:
# export LOCALE_ARCHIVE=$(nix-build '<nixpkgs>' -A glibcLocales)/lib/locale/locale-archive

# to fix issue: 'home-manager: line 73: NIX_PATH: unbound variable'
# export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH

let
  my-dotfile-dir = "/home/dono/dotfiles";
  my-home-dir = "/home/dono";
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  nixpkgs.overlays = [
    (import (builtins.fetchGit {
      url = "git://github.com/nix-community/emacs-overlay.git";
      rev = "b326ce0166ca52223e8efeae8a3763c00cca1ba4";
    }))
  ];

  home.packages = with pkgs; [

    #terminal

    wget
    vim
    curl
    git
    ((emacsPackagesFor emacsGit).emacsWithPackages
      (epkgs: [ epkgs.vterm epkgs.w3m ]))
    emacs-all-the-icons-fonts
    nerdfonts
    zsh
    networkmanager
    cachix
    lorri
    nodejs

    powershell

    tmux
    zellij
    ranger
    fzf
    ripgrep
    tig
    lazygit

    stack
    cargo
    sbcl

    gnupg
    pass

    mu
    # notmuch
    # notmuch-bower
    isync
    cacert

    bat
    exa
    fd

    cmus
    pandoc

    xclip
    powerline-fonts

    tuir # rtv
    ddgr
    w3m
    youtube-dl

    youtube-viewer
    urlview

    nix-prefetch-git
    dropbox
    (import ../nix/twitchy.nix)
    (import ../nix/twitchy-rofi-script.nix)
    (import ../nix/search-ddg-script.nix)
    (import ../nix/twitchy-play-emacs.nix)

    cmatrix
    ormolu
    nixfmt
    closurecompiler
    nodejs
    cmake
    libvterm
    libtool
    rdrview

    awscli

    (haskellPackages.greenclip)
    bluez
    bluez-tools

    jdk8

    tts

    #graphical

    # kdeconnect

    firefox-bin
    xterm
    gparted
    chromium
    google-chrome
    nyxt

    mplayer
    alsaLib
    ffmpeg
    mpv
    vlc
    pavucontrol
    pasystray
    playerctl

    (spotifyd.override {
      withALSA = false;
      withPulseAudio = true;
    })

    rofi
    dmenu
    conky

    guvcview

    xscreensaver
    feh

    gnome3.gedit
    gnome3.gnome-system-monitor
    pkgs.meld
    xfce.thunar

    streamlink

    dolphinEmu
    mupen64plus
    keepassx

    vscode
    protontricks

    blueman

    darktable

    android-studio
    scrcpy

    redshift
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

 programs.zsh = {
   enable = true; # had to disable to make home-manager switch work...
   enableAutosuggestions = true;
   # enableSyntaxHighlighting = true; #remove this line if not compile
   initExtra =
     "if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi";

   oh-my-zsh = {
     enable = true;
     theme = "agnoster";
     plugins = [ "git" ];
   };

   shellAliases = { cls = "clear"; };
   sessionVariables = { RIPGREP_CONFIG_PATH = "${my-home-dir}/.ripgreprc"; };
 };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.tmux = {
    enable = true;
    clock24 = true;
    plugins = with pkgs.tmuxPlugins; [
        sensible
        yank
        tmux-fzf
        {
        plugin = dracula;
        extraConfig = ''
            set -g @dracula-show-battery false
            set -g @dracula-show-powerline true
            set -g @dracula-refresh-rate 10
        '';
        }
    ];

    extraConfig = ''
        unbind C-b
        set-option -g prefix C-a
        bind-key C-a send-prefix

        #map F5 to cycle to next window
        bind -n F5 next-window

        #map F6 to cycle to next pane
        bind -n F6 select-pane -t :.+

        set -g mouse on
    '';
  };

  programs.home-manager = { enable = true; };

  home.file.".vimrc".source = "${my-dotfile-dir}/vimrc";
  home.file.".Xresources".source = "${my-dotfile-dir}/Xresources";
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  home.file.".xmonad/xmonad.hs".source = "${my-dotfile-dir}/.xmonad/xmonad.hs";
  home.file.".ghci".source = "${my-dotfile-dir}/.ghci";
  home.file.".ripgreprc".source = "${my-dotfile-dir}/.ripgreprc";
}
