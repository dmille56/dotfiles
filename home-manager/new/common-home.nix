{ pkgs, config, lib, ... }:
let 
  constants = import ./common-constants.nix; 
in with constants;
{
  # :NOTE: Lists automatically merge, so this all gets added to home.nix's packages
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
  ];

  sops = { 
    # :NOTE: generate key with age:
    # mkdir -p ~/.config/sops/age
    # age-keygen -o ~/.config/sops/age/keys.txt
    # to output the public key: age-keygen -y ~/.config/sops/age/keys.txt
    # change the yaml config at .sops.yaml (when need to update the age key)
    # to update the keys in the yaml config: sops updatekeys .sops.yaml
    age.keyFile = lib.mkDefault "${my-home-dir}/.config/sops/age/keys.txt"; # Path to your age key file
    # :NOTE:
    # to add/init a new secrets file: 
    # sops secrets.yaml
    defaultSopsFile = lib.mkDefault ../../secrets.yaml; # default secrets file
    
    # :NOTE: need to add one of these entries for each secret added to the secrets file (so can be accessed in nix)
    secrets.OPENAI_API_KEY.path = lib.mkDefault "${config.sops.defaultSymlinkPath}/OPENAI_API_KEY";
    secrets.GOOGLE_API_KEY.path = lib.mkDefault "${config.sops.defaultSymlinkPath}/GOOGLE_API_KEY";
    secrets.ANTHROPIC_API_KEY.path = lib.mkDefault "${config.sops.defaultSymlinkPath}/ANTHROPIC_API_KEY";
    secrets.GIT_NAME.path = lib.mkDefault "${config.sops.defaultSymlinkPath}/GIT_NAME";
    secrets.GIT_EMAIL.path = lib.mkDefault "${config.sops.defaultSymlinkPath}/GIT_EMAIL";
    secrets.GITHUB_USER.path = lib.mkDefault "${config.sops.defaultSymlinkPath}/GITHUB_USER";
  };

  # :NOTE: lib.mkDefault makes it so the setting can be overrun in home.nix
  home.sessionVariables = {
    OPENAI_API_KEY = lib.mkDefault "$(cat ${config.sops.secrets.OPENAI_API_KEY.path})";
    GOOGLE_API_KEY = lib.mkDefault "$(cat ${config.sops.secrets.GOOGLE_API_KEY.path})";
    ANTHROPIC_API_KEY = lib.mkDefault "$(cat ${config.sops.secrets.ANTHROPIC_API_KEY.path})";
    OPENAI_API_MODEL = lib.mkDefault "gpt-5-mini";
    AIDER_MODEL = lib.mkDefault "gpt-5-mini";
    RIPGREP_CONFIG_PATH = lib.mkDefault "${my-home-dir}/.ripgreprc";
    LG_CONFIG_FILE= lib.mkDefault "${my-home-dir}/.config/lazygit/config.yml,${my-home-dir}/.config/lazygit/theme/lazygit/themes-mergable/mocha/blue.yml";
    BROWSER = lib.mkDefault "${pkgs.firefox-bin}/bin/firefox";
    TERMINAL_EMULATOR = lib.mkDefault "${pkgs.alacritty}/bin/alacritty";
    TERMINAL = lib.mkDefault "${pkgs.alacritty}/bin/alacritty";
    PAGER = lib.mkDefault "less";
    EDITOR = lib.mkDefault "nvim";
    VISUAL = lib.mkDefault "neovide";
    NIXPKGS_ALLOW_UNFREE = lib.mkDefault "1";
    MY_MACHINE_ID = lib.mkDefault "none";
  };
}
