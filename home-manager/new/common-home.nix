{ pkgs, lib, ... }:
{

  # Lists automatically merge, so this all gets added to home.nix's packages
  home.packages = with pkgs; [
    #terminal
    nano
    micro
    kakoune
    helix
    vim
    # vimgolf
    # pacvim
    curl
    git
    git-credential-oauth
    ((emacsPackagesFor emacs-gtk).emacsWithPackages
      (epkgs: [ epkgs.vterm epkgs.w3m epkgs.jinx ]))
    eask-cli #emacs eask
    zsh
    networkmanager
    lorri
    python313Packages.python-lsp-server
    python313Packages.ruff
    yaml-language-server
    typescript-language-server
    
    # fonts
    emacs-all-the-icons-fonts
    font-awesome
    
    # minimal nerd fonts
    nerd-fonts.symbols-only
    nerd-fonts.meslo-lg
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    nerd-fonts.hack
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.noto

    # optional nerd fonts
    nerd-fonts.caskaydia-mono
    nerd-fonts.roboto-mono
    nerd-fonts.ubuntu-mono
    nerd-fonts.ubuntu
    nerd-fonts.inconsolata
    nerd-fonts.inconsolata-go
    nerd-fonts.inconsolata-lgc
    nerd-fonts.proggy-clean-tt
    nerd-fonts.terminess-ttf
    nerd-fonts.sauce-code-pro
    nerd-fonts.iosevka-term-slab
    nerd-fonts.fira-mono
    
    # ubuntu font
    ubuntu-classic

    powershell
    tmux
    ranger
    yazi
    fzf
    ripgrep

    # haskell
    stack
    ormolu
    
    # rust
    cargo

    gnupg
    pass
    age
    sops

    bat
    eza #exa
    fd
    bottom
    delta

    cmus
    pandoc

    xclip
    powerline-fonts

    warpd

    # ai
    aider-chat-full
    claude-code
    # ollama
    
    sqlite-interactive

    tuir # rtv
    ddgr
    buku
    w3m
    ncdu
    youtube-tui
    yt-dlp
    ytfzf
    piper-tts
    # csvtool
    ueberzugpp 
    lazygit
    git-repo-updater #gitup
    fastfetch

    # (import ../../nix/twitchy.nix) # :TODO: fix 
    # (import ../../nix/twitchy-rofi-script.nix)
    (import ../../nix/search-ddg-script.nix)
    # (import ../nix/twitchy-play-emacs.nix)
    # (import ../nix/ChatGPT/ChatGPT-CLI.nix)
    (import ../../nix/play-yt-script.nix)
    (import ../../nix/play-yt-script-format.nix)
    (import ../../nix/rofi-buku.nix)
    (import ../../nix/my-tts.nix)
    (import ../../nix/trayer-padding-icon.nix { inherit stdenv; })
    (import ../../nix/fireplace.nix { inherit lib stdenv fetchgit ncurses5; })

    cmatrix
    neo
    snowmachine
    lolcat
    nixfmt
    libvterm
    libtool

    awscli

    (haskellPackages.greenclip)
    bluez
    bluez-tools

    # graphical apps

    firefox-bin
    chromium
    google-chrome

    # terminal editors
    xterm
    # termonad # :TODO: figure out why this broke
    alacritty
    neovide

    mplayer
    alsa-lib
    ffmpeg-full
    mpv
    vlc
    freetube
    pavucontrol
    pasystray
    trayer
    playerctl

    # rofi
    rofi-bluetooth 
    rofi-power-menu
    yad # :NOTE: to make sys tray program for rofi-power-menu
    wmctrl # :NOTE: to make rofi-games work with steam properly

    xdotool
    noto-fonts-color-emoji

    gmrun
    dmenu
    
    #video editing
    # shotcut

    realvnc-vnc-viewer
    gimp-with-plugins
    flameshot
    # obs-studio

    xscreensaver
    feh
    xmobar

    lite-xl
    gnome-system-monitor
    pkgs.meld
    thunar
    pcmanfm

    streamlink

    # dolphin-emu
    # mupen64plus
    # snes9x-gtk
    keepassxc
    spotify
    spotify-player
    blanket

    vscode
    blueman
    # darktable
    scrcpy
    redshift

    hello # :TODO: remove me eventually
  ];

  home.sessionVariables = {
    # Using mkDefault allows laptop-home.nix to overwrite these without conflicts
    MY_MACHINE_ID = lib.mkDefault "none";
    HELLO_WORLD_VAR = lib.mkDefault "hello world";
  };
}
