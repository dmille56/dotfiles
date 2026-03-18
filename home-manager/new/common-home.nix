{ pkgs, config, lib, ... }:
let 
  constants = import ./common-constants.nix; 
in with constants;
{
  # :NOTE: misc settings

  programs.home-manager.enable = true; # obviously we need to enable home-manager

  # :NOTE: lib.mkDefault makes it so the setting can be overrun in home.nix
  xsession.enable = lib.mkDefault true;
  fonts.fontconfig.enable = lib.mkDefault true;

  home.username = lib.mkDefault "${my-username}";
  home.homeDirectory = lib.mkDefault "${my-home-dir}";
  home.stateVersion = lib.mkDefault "23.11"; # To figure this out you can comment out the line and see what version it expected.

  # :NOTE: enable automatic nix garbage collection
  nix.gc = {
    automatic = lib.mkDefault true;
    dates = lib.mkDefault "daily";
    # Optional: options to pass to nix-collect-garbage, e.g., "--delete-older-than 30d"
    options = lib.mkDefault "--delete-older-than 14d";
  };

  # :NOTE: packages config
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
    # zsh
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

  # :NOTE: programs config starts here

  programs.fzf = {
    enable = lib.mkDefault true;
    enableZshIntegration = lib.mkDefault true;
  };

  programs.direnv = {
    enable = lib.mkDefault true;
    enableZshIntegration = lib.mkDefault true;
  };

  programs.zoxide = {
    enable = lib.mkDefault true;
    enableZshIntegration = lib.mkDefault true;
  };

  programs.rofi = {
    enable = lib.mkDefault true;
    plugins = lib.mkDefault (with pkgs; [ 
      rofi-emoji
      rofi-games
    ]);
    # theme = "glue_pro_blue"; #good fallback theme that comes installed with rofi
    theme = lib.mkDefault "dracula-theme";
  };
  
  programs.delta = {
    enable = lib.mkDefault true;
    enableGitIntegration = lib.mkDefault true;
    options = {
      features = lib.mkDefault "dracula";
    };
  };

  programs.git = {
    enable = lib.mkDefault true;
    settings = {
      # :TODO: fix these
      # github.user = "$(cat ${config.sops.secrets.GITHUB_USER.path})";
      # user.name = "$(cat ${config.sops.secrets.GIT_NAME.path})";
      # user.email = "$(cat ${config.sops.secrets.GIT_EMAIL.path})";
      github.user = lib.mkDefault "dmille56";
      user.name = lib.mkDefault "Donovan M";
      user.email = lib.mkDefault "donovanm256@gmail.com";
      color = {
        ui = lib.mkDefault "always";
      };
      alias = {
        st = lib.mkDefault "status";
        ci = lib.mkDefault "commit"; 
        br = lib.mkDefault "branch";
        co = lib.mkDefault "checkout";
      };
      credential.helper = lib.mkDefault "oauth";
    };
  };
  
  programs.gh = {
    enable = lib.mkDefault true;
    extensions = lib.mkDefault [ pkgs.gh-dash ];
    gitCredentialHelper.enable = lib.mkDefault true;
  };

  programs.tmux = {
    enable = lib.mkDefault true;
    clock24 = lib.mkDefault true;
    plugins = lib.mkDefault (with pkgs.tmuxPlugins; [
      sensible
      yank
      tmux-fzf
      jump
      urlview
      {
        plugin = dracula;
        extraConfig = ''
            set -g @dracula-show-battery false
            set -g @dracula-show-powerline true
            set -g @dracula-refresh-rate 10
        '';
      }
    ]);

    extraConfig = lib.mkDefault ''
        unbind C-b
        set-option -g prefix C-a
        bind-key C-a send-prefix

        #map F5 to cycle to next window
        bind -n F5 next-window

        #map F6 to cycle to next pane
        bind -n F6 select-pane -t :.+

        set -g mouse on

        # Start windows and panes at 1, not 0
        set -g base-index 1
        setw -g pane-base-index 1

        # Make % (horizontal split) and " (vertical split) bindings split the window and cd to the current path
        bind % split-window -h -c "#{pane_current_path}"
        bind '"' split-window -v -c "#{pane_current_path}"
    '';
  };

  # :TODO: fix this to make zsh options all optional with lib.mkDefault
  programs.zsh = {
    enable = true;
    #autosuggestion.enable = true;

    initContent = ''
     if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi
     source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
   '';

    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "git" ];
    };

    shellAliases = {
      cls = "clear";
      r = "ranger";
      nv = "nvim";
      lg = "lazygit";
      e = "emacs";
    };

    plugins = [
      {
        name = "fzf-tab";
        src = pkgs.fetchFromGitHub {
          owner = "Aloxaf";
          repo = "fzf-tab";
          rev = "fac145167f7ec1861233c54de0c8900b09c650fe";
          sha256 = "1Ior+/9e+M+Fc1u0uq5HhknlGRS96q7tazhEE6rmx9Y=";
        };
      }
    ];

    localVariables = {
      ZSH_DISABLE_COMPFIX = "true"; # for syntax highlighting to work
    };
    # sessionVariables = { RIPGREP_CONFIG_PATH = "${my-home-dir}/.ripgreprc"; };
  };

  # :NOTE: services config starts here

  services.kdeconnect.enable = lib.mkDefault true;
  services.syncthing.enable = lib.mkDefault true;

  services.spotifyd = {
    enable = lib.mkDefault true;
    package = lib.mkDefault (pkgs.spotifyd.override { withPulseAudio = true; withMpris = true; });
    settings.global = {
      username = lib.mkDefault "donovanm56";
      password_cmd = lib.mkDefault "pass spotify";
      backend = lib.mkDefault "pulseaudio";
      cache_path = lib.mkDefault "${my-home-dir}/.cache/spotifyd";
      max_cache_size = lib.mkDefault 2000000000;
      bitrate = lib.mkDefault 160;
    };
  };

  services.lorri = {
    enable = lib.mkDefault true;
    enableNotifications = lib.mkDefault true;
  };

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
