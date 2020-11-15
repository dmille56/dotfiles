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

let
  my-dotfile-dir = "/home/dono/dotfiles";
  my-home-dir = "/home/dono";
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.0.2u" "p7zip-16.02" ];

  home.sessionVariables.LOCALES_ARCHIVE =
    "${pkgs.glibcLocales}/lib/locale/locale-archive";

  nixpkgs.overlays = [
    (import (builtins.fetchGit {
      url = "git://github.com/nix-community/emacs-overlay.git";
      rev = "853e4e31a95ca9a6fd5f08c7a94238ddf19ecb4e";
    }))
  ];

  home.packages = with pkgs; [

    #terminal

    wget
    vim
    curl
    git
    ((emacsPackagesNgGen emacsGit).emacsWithPackages
      (epkgs: [ epkgs.vterm epkgs.w3m ]))
    emacs-all-the-icons-fonts
    zsh
    networkmanager
    cachix

    powershell
    tmux
    ranger
    fzf
    ripgrep
    tig
    lazygit

    stack
    cargo

    gnupg
    pass

    mu
    notmuch
    notmuch-bower
    isync
    cacert

    bat
    exa
    fd

    cmus
    pandoc

    xclip
    powerline-fonts

    rtv
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

    awscli

    (haskellPackages.greenclip)

    #graphical

    #kdeconnect

    firefox-bin
    xterm
    gparted
    chromium
    google-chrome

    mplayer
    alsaLib
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

    xscreensaver
    feh

    gnome3.gedit
    gnome3.gnome-system-monitor
    gnome3.meld
    xfce.thunar

    streamlink

    dolphinEmu
    mupen64plus
    keepassx

    vscode
    protontricks

    # redshift
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    # enableSyntaxHighlighting = true; #remove this line if not compile
    initExtra =
      "if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi";

    oh-my-zsh = {
      enable = true;
      theme = "sunaku";
      plugins = [ "git" ];
    };

    shellAliases = { cls = "clear"; };
    sessionVariables = { RIPGREP_CONFIG_PATH = "${my-home-dir}/.ripgreprc"; };
  };

  programs.home-manager = { enable = true; };

  home.file.".tmux.conf".source = "${my-dotfile-dir}/tmux.conf";
  home.file.".vimrc".source = "${my-dotfile-dir}/vimrc";
  home.file.".Xresources".source = "${my-dotfile-dir}/Xresources";
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  home.file.".xmonad/xmonad.hs".source = "${my-dotfile-dir}/.xmonad/xmonad.hs";
  home.file.".ghci".source = "${my-dotfile-dir}/.ghci";
  home.file.".ripgreprc".source = "${my-dotfile-dir}/.ripgreprc";
}
