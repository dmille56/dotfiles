# sudo nixos-rebuild switch
{ pkgs, config, lib, ...}: 

# :NOTE: update nix pkgs
# cd /etc/nixos && sudo nix flake update

# :NOTE: Garbage collect nix:
# nix-collect-garbage -d

# Install nix files in /etc/nix 
# - Copy configuration.nix, home.nix, flake.nix to /etc/nix

# Non nix install:
# 1. setup syncthing directories
# 2. setup firefox / chrome extensions
#   - vimium, ublock origin, stylus, location guard, vihn, old reddit redirect, reddit enhancement suite, what hacker news says, dracula theme, blocktube, unhook
# 3. setup vscode extensions
#   - python, vscode neovim, direnv, dracula theme, grafana alloy, haskell, nix, c#

let
  username = "dono";
  my-dotfile-dir = "/home/${username}/dotfiles";
  my-home-dir = "/home/${username}";
  sweetIconsRepo = builtins.fetchGit {
    url = "https://github.com/EliverLara/Sweet-folders";
    rev = "40a5d36e50437901c7eaa1119bb9ae8006e2fe5c";
  };
  draculaWallpaperRepo = builtins.fetchGit {
    url = "https://github.com/dracula/wallpaper";
    rev = "f2b8cc4223bcc2dfd5f165ab80f701bbb84e3303";
  };
in
{
  imports = [ ./common-home.nix ];

  home.username = "${username}";
  home.homeDirectory = "${my-home-dir}";
  home.stateVersion = "23.11"; # To figure this out you can comment out the line and see what version it expected.
  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  xsession.enable = true;

  # :NOTE: enable automatic nix garbage collection
  nix.gc = {
    automatic = true;
    dates = "daily";
    # Optional: options to pass to nix-collect-garbage, e.g., "--delete-older-than 30d"
    options = "--delete-older-than 14d";
  };

  home.packages = lib.unique (config.home.packages ++ (with pkgs; [
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
    
    # nixgl.auto.nixGLDefault # allows running OpenGL applications via nix on non nix-os systems

    #graphical

    firefox-bin
    xterm
    # termonad # :TODO: figure out why this broke
    alacritty
    neovide
    chromium
    google-chrome

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

    steam # :NOTE: added
    sensible-utils # :NOTE: added
    xdg-utils # :NOTE: added
    libnotify # :NOTE: added
    wifitui # :NOTE: added
    zoom-us # :NOTE: added
    remmina # :NOTE: added
    xmessage # :NOTE: added
    x2goclient # :NOTE: added
  ]));

  # :TODO: make sure this works to set gtk applications themes correctly
  # gtk = {
  #   enable = true;
  #   theme = {
  #     name = "dracula";
  #     # package = pkgs.dracula-theme;
  #   };
  #   iconTheme = {
  #     name = "candy-icons";
  #     # package = pkgs.papirus-icon-theme;
  #   };
  # };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

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

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.rofi = {
    enable = true;
    # :TODO: figure out why the plugins aren't working correctly
    plugins = with pkgs; [ 
      rofi-emoji
      rofi-games
    ];
    # theme = "glue_pro_blue"; #good fallback theme that comes installed with rofi
    theme = "dracula-theme";
  };
  
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      features = "dracula";
    };
  };

  programs.git = {
    enable = true;
    settings = {
      # :TODO: fix these
      # github.user = "$(cat ${config.sops.secrets.GITHUB_USER.path})";
      # user.name = "$(cat ${config.sops.secrets.GIT_NAME.path})";
      # user.email = "$(cat ${config.sops.secrets.GIT_EMAIL.path})";
      github.user = "dmille56";
      user.name = "Donovan M";
      user.email = "donovanm256@gmail.com";
      color = {
        ui = "always";
      };
      alias = {
        st = "status";
        ci = "commit"; 
        br = "branch";
        co = "checkout";
      };
      credential.helper = "oauth";
    };
  };
  
  programs.gh = {
    enable = true;
    extensions = [ pkgs.gh-dash ];
    gitCredentialHelper.enable = true;
  };

  programs.tmux = {
    enable = true;
    clock24 = true;
    plugins = with pkgs.tmuxPlugins; [
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

        # Start windows and panes at 1, not 0
        set -g base-index 1
        setw -g pane-base-index 1

        # Make % (horizontal split) and " (vertical split) bindings split the window and cd to the current path
        bind % split-window -h -c "#{pane_current_path}"
        bind '"' split-window -v -c "#{pane_current_path}"
    '';
  };

  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-which-key
      direnv-vim
      dracula-vim
      nerdtree
      ctrlp-vim
      vim-airline
      nvim-lspconfig
      vim-easymotion
      telescope-nvim
      telescope-zoxide
      neogit
      golden-ratio
      lazygit-nvim
      orgmode

      # completion
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      nvim-cmp
      cmp-vsnip
      vim-vsnip
      
      #ai
      codecompanion-nvim

      vim-be-good
    ];

    extraConfig = ''
    set nocompatible

    syntax enable

    "get rid of annoyances
    set noswapfile
    set nobackup
    set nowritebackup

    set ignorecase          " Make searching case insensitive
    set smartcase           " ... unless the query has capital letters.
    set gdefault            " Use 'g' flag by default with :s/foo/bar/.

    " set color scheme
    colorscheme dracula
    set termguicolors

    "remap ; to : to save a keystroke
    nnoremap : ;
    nnoremap ; :
    vnoremap : ;
    vnoremap ; :
    
    " Use system clipboard for all yank/delete/put operations
    set clipboard+=unnamedplus

    let mapleader="\<SPACE>"

    "NERDTree
    "-------------------------
    nnoremap <leader>\ :NERDTreeToggle<CR>
    nnoremap <leader>uy :NERDTreeToggle<CR>
    let NERDTreeIgnore = [ '\.js_dyn_o', '\.js_hi', '\.js_o', '\.js_dyn_hi', '\.dyn_hi', '\.dyn_o', '\.hi', '\.o', '\.p_hi', '\.p_o' ]
    "Automatically close if NERDTree is the only buffer left
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
    
    " Open file menu
    " nnoremap <Leader>p :CtrlP<CR>
    " Open buffer menu
    " nnoremap <Leader>b :CtrlPBuffer<CR>
    " Open most recently used files
    nnoremap <Leader>P :CtrlPMRUFiles<CR>

    " Move to word
    map  <Leader>w <Plug>(easymotion-bd-w)
    nmap <Leader>w <Plug>(easymotion-overwin-w)

    " <Leader>j{char} to move to {char}
    map  <Leader>J <Plug>(easymotion-bd-f)
    nmap <Leader>J <Plug>(easymotion-overwin-f)
    map  <Leader>f <Plug>(easymotion-bd-f)
    nmap <Leader>f <Plug>(easymotion-overwin-f)

    " Move to line
    map <Leader>j <Plug>(easymotion-bd-jk)
    nmap <Leader>j <Plug>(easymotion-overwin-line)

    " CTRL-Tab is next tab
    noremap <C-Tab> :<C-U>tabnext<CR>
    inoremap <C-Tab> <C-\><C-N>:tabnext<CR>
    cnoremap <C-Tab> <C-C>:tabnext<CR>
    " CTRL-SHIFT-Tab is previous tab
    noremap <C-S-Tab> :<C-U>tabprevious<CR>
    inoremap <C-S-Tab> <C-\><C-N>:tabprevious<CR>
    cnoremap <C-S-Tab> <C-C>:tabprevious<CR>

    " New tab keybinding
    nnoremap <Leader>tn :tabnew<CR>

    " Map go to tab keybindings
    nnoremap <leader>t1 1gt<CR>
    nnoremap <leader>t2 2gt<CR>
    nnoremap <leader>t3 3gt<CR>
    nnoremap <leader>t4 4gt<CR>
    nnoremap <leader>t5 5gt<CR>
    nnoremap <leader>t6 6gt<CR>
    nnoremap <leader>t7 7gt<CR>
    nnoremap <leader>t8 8gt<CR>
    nnoremap <leader>t9 9gt<CR>
    nnoremap <leader>t0 10gt<CR>

    " :TODO: figure out a keybinding for fuzzy deleting buffers
    " :TODO: add toggleterm package and keybindings https://github.com/akinsho/toggleterm.nvim
    " :TODO: look into integrations with tmux https://github.com/aserowy/tmux.nvim
    " :TODO: add harpoon plugin and keybindings https://github.com/ThePrimeagen/harpoon/tree/harpoon2
    " :TODO: look into this plugin https://github.com/junegunn/fzf.vim
    
    " Control+W followed by W
    nnoremap <leader>o <C-w>w<CR>
    nnoremap <leader>2 <C-w>s<CR>
    nnoremap <leader>3 <C-w>v<CR>

    nnoremap <leader>i :Telescope current_buffer_fuzzy_find<CR>
    nnoremap <leader>us :Telescope live_grep<CR>
    nnoremap <leader>p :Telescope find_files<CR>
    nnoremap <leader>b :Telescope buffers<CR>

    nnoremap <leader>g :Neogit<CR>
    nnoremap <leader>G :LazyGit<CR>

    nnoremap <leader>ug :CodeCompanionChat<CR>

    nnoremap <leader>z :Telescope zoxide list<CR>

    " Make terminal ESC work like you would expect it to
    tnoremap <Esc> <C-\><C-n>

    let g:airline_powerline_fonts = 1
    let g:airline_theme= 'dracula'

    set completeopt=menu,menuone,noselect

    lua <<EOF

      -- Setup neogit
      local neogit = require'neogit'
      neogit.setup()

      -- Setup orgmode
      require('orgmode').setup({
        org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
        org_default_notes_file = '~/Dropbox/org/refile.org',
      })

      -- Setup nvim-cmp.
      local cmp = require'cmp'

      cmp.setup {
          mapping = {
              ["<C-d>"] = cmp.mapping.scroll_docs(-4),
              ["<C-f>"] = cmp.mapping.scroll_docs(4),
              ['<C-k>'] = cmp.mapping.select_prev_item(select_opts),
              ['<C-j>'] = cmp.mapping.select_next_item(select_opts),
              ['<Up>'] = cmp.mapping.select_prev_item(select_opts),
              ['<Down>'] = cmp.mapping.select_next_item(select_opts),
              ["<C-e>"] = cmp.mapping.close(),
              ['<C-Space>'] = cmp.mapping.complete(),
              ['<C-e>'] = cmp.mapping.abort(),
              ["<CR>"] = cmp.mapping.confirm {
                  behavior = cmp.ConfirmBehavior.Insert,
                  select = true,
              },
          },

          snippet = {
              -- REQUIRED - you must specify a snippet engine
              expand = function(args)
                  vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
              end,
          },
  
          sources = {
              { name = "nvim_lsp"},
              { name = "path" },
              { name = 'vsnip' },
              { name = "buffer" , keyword_length = 5},
          },
          experimental = {
              ghost_text = true
          }
      }

      -- Setup lspconfig.
      vim.lsp.enable('pylsp')
      vim.lsp.enable('ruff')
      vim.lsp.enable('ts_ls')
      vim.lsp.enable('hls')

      -- Setup AI codecompanion
      require("codecompanion").setup({
        strategies = {
          chat = {
            adapter = "openai",
            model = "gpt-5-mini"
          },
        }
      })
    EOF
    '';
  };
  
  sops = { 
    # :NOTE: generate key with age:
    # mkdir -p ~/.config/sops/age
    # age-keygen -o ~/.config/sops/age/keys.txt
    # to output the public key: age-keygen -y ~/.config/sops/age/keys.txt
    # change the yaml config at .sops.yaml (when need to update the age key)
    age.keyFile = "${my-home-dir}/.config/sops/age/keys.txt"; # Path to your age key file
    # :NOTE:
    # to add/init a new secrets file: 
    # sops secrets.yaml
    defaultSopsFile = ../../secrets.yaml; # default secrets file
    
    # :NOTE: need to add one of these entries for each secret added to the secrets file (so can be accessed in nix)
    secrets.OPENAI_API_KEY.path = "${config.sops.defaultSymlinkPath}/OPENAI_API_KEY";
    secrets.GOOGLE_API_KEY.path = "${config.sops.defaultSymlinkPath}/GOOGLE_API_KEY";
    secrets.ANTHROPIC_API_KEY.path = "${config.sops.defaultSymlinkPath}/ANTHROPIC_API_KEY";
    secrets.GIT_NAME.path = "${config.sops.defaultSymlinkPath}/GIT_NAME";
    secrets.GIT_EMAIL.path = "${config.sops.defaultSymlinkPath}/GIT_EMAIL";
    secrets.GITHUB_USER.path = "${config.sops.defaultSymlinkPath}/GITHUB_USER";
  };

  services.kdeconnect.enable = true;
  services.syncthing.enable = true;

  services.spotifyd = {
    enable = true;
    package = (pkgs.spotifyd.override { withPulseAudio = true; withMpris = true; });
    settings.global = {
      username = "donovanm56";
      password_cmd = "pass spotify";
      backend = "pulseaudio";
      cache_path = "${my-home-dir}/.cache/spotifyd";
      max_cache_size = 2000000000;
      bitrate = 160;
    };
  };

  services.lorri = {
    enable = true;
    enableNotifications = true;
  };

  home.file.".config/kak/kakrc".text = ''
    colorscheme dracula
  '';

  home.file.".config/helix/config.toml".text = ''
    theme = "dracula"
  '';

  home.file.".config/alacritty/alacritty.toml".text = ''
    general.import = ["~/.config/alacritty/dracula.toml"]

    [font]
    size = 13.0
    
    [font.bold]
    family = "DejaVu Sans Mono"
    style = "Bold"
    
    [font.bold_italic]
    family = "DejaVu Sans Mono"
    style = "Bold Italic"
    
    [font.italic]
    family = "DejaVu Sans Mono"
    style = "Italic"
    
    [font.normal]
    family = "DejaVu Sans Mono"
    style = "Regular"

    [window]
    opacity = 0.8
  '';
  
  home.file.".config/spotifyd/spotifyd.conf".text = ''
    [global]
    username = "donovanm56"
    password_cmd = "pass spotify"
    backend = "pulseaudio"
    cache_path = "${my-home-dir}/.cache/spotifyd"
    max_cache_size = 2000000000
    bitrate = 160
  '';

  home.file.".config/spotify-player/app.toml".source = "${my-dotfile-dir}/.config/spotify-player/app.toml";

  home.file.".config/rofi-games/config.toml".text = ''
    [launchers.steam]
    extra_args = ["-silent"]
  '';

  home.file.".vimrc".source = "${my-dotfile-dir}/vim/vim/vimrc";
  home.file.".Xresources".source = "${my-dotfile-dir}/Xresources";
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc-laptop";
  home.file.".xmonad/xmonad.hs".source = "${my-dotfile-dir}/.xmonad/xmonad.hs";
  home.file.".xmonad/lib/MyTheme.hs".source = "${my-dotfile-dir}/.xmonad/MyTheme.hs";
  home.file.".ghci".source = "${my-dotfile-dir}/.ghci";
  home.file.".ripgreprc".source = "${my-dotfile-dir}/.ripgreprc";
  home.file.".config/mpv/mpv.conf".source = "${my-dotfile-dir}/mpv.conf";
  home.file.".config/ytfzf/conf.sh".source = "${my-dotfile-dir}/ytfzf-conf.sh";
  home.file.".config/termonad/termonad.hs".source = "${my-dotfile-dir}/.termonad/termonad.hs";
  home.file.".config/redshift.conf".source = "${my-dotfile-dir}/redshift.conf";
 
  home.file.".config/rofi/themes/dracula-theme.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/rofi/48a024639fbf25e3237766f0dcef4af75a2df908/theme/config1.rasi";
    sha256 = "52f26dd7c44bb919a7a604d71bea26df5e52bd2188f9804e103fc002239bc99a";
  });

  home.file.".config/rofi/themes/dracula-theme-2.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/rofi/48a024639fbf25e3237766f0dcef4af75a2df908/theme/config2.rasi";
    sha256 = "68010556ad7b351b63b6d061f5c4b7c8feb9d9b32687bf0530b105a86634766c";
  });

  home.file.".config/rofi/themes/games-default.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/Rolv-Apneseth/rofi-games/17e53726e8f81f6bbe15b4dd66329f869409f4c6/themes/games-default.rasi";
    sha256 = "sha256:0w3cyp7v102n5pdmngz1rhzh86rrc120gfxzxv3h86vs1ch2zli2";
  });

  home.file.".config/rofi/themes/games-smaller.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/Rolv-Apneseth/rofi-games/17e53726e8f81f6bbe15b4dd66329f869409f4c6/themes/games-smaller.rasi";
    sha256 = "sha256:1105kf4q9flrdpcf0mzzav6h8m2maj9yfrm9avzwkdfcddax6i7s";
  });

  home.file.".config/kak/colors/dracula.kak".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/kakoune/master/colors/dracula.kak";
    sha256 = "57e11ca24375df2a02541a44670d4a48cf90ce179f4eecfc0f5ba005c2c03a02";
  });

  home.file.".themes/dracula".source = builtins.fetchGit {
    url = "https://github.com/dracula/gtk";
    rev = "f3c876d8c97f9bb504c98592a8d96770e70585bb";
  };

  home.file.".themes/sweet".source = builtins.fetchTarball {
    url = "https://github.com/EliverLara/Sweet/releases/download/v6.0/Sweet.tar.xz";
    sha256 = "sha256:012nc14vz31zq34pg5nv2ybzryhqpf6x0jh022nvgyhrf9q1zrn9";
  };

  home.file.".themes/sweet-dark".source = builtins.fetchTarball {
    url = "https://github.com/EliverLara/Sweet/releases/download/v6.0/Sweet-Dark.tar.xz";
    sha256 = "sha256:1j67jwpab7f13rgcxmzqqvjixp5j8mdpv6vpsf2ajnh5yyxzqxvj";
  };

  home.file.".local/share/icons/candy-icons".source = builtins.fetchGit {
    url = "https://github.com/EliverLara/candy-icons";
    rev = "83512fbcadcb7e1015ebbe1729a1894946b021be";
  };

  home.file.".local/share/icons/Sweet-Purple" = {
    source = "${sweetIconsRepo}/Sweet-Purple";
    recursive = true;
  };

  home.file.".local/share/icons/Sweet-Rainbow" = {
    source = "${sweetIconsRepo}/Sweet-Rainbow";
    recursive = true;
  };

  # :NOTE: this sets the desktop wallpaper
  home.file.".background-image" = {
    source = "${draculaWallpaperRepo}/first-collection/nixos.png";
  };

  home.file.".local/share/xfce4/terminal/colorschemes/Dracula.theme".source = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/xfce4-terminal/refs/heads/master/Dracula.theme";
  };
  
  home.file.".config/lazygit/theme/lazygit".source = builtins.fetchGit {
    url = "https://github.com/catppuccin/lazygit";
    rev = "a544cef9a18c3a94e0344281e0ddcf99a18a8ede";
  };
  home.file.".config/lazygit/config.yml".text = ''
  '';

  home.file.".config/alacritty/dracula.toml".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/alacritty/master/dracula.toml";
    sha256 = "e9de3a792548c8112168c1dd18b5651d1ebee2893975cda4ccd9c4c0430c87b8";
  });

  home.file."piper/models/en_US-lessac-high.onnx".source = builtins.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/lessac/high/en_US-lessac-high.onnx?download=true";
    sha256 = "4cabf7c3a638017137f34a1516522032d4fe3f38228a843cc9b764ddcbcd9e09";
  };

  home.file."piper/models/en_US-lessac-high.onnx.json".text = builtins.readFile(builtins.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/raw/main/en/en_US/lessac/high/en_US-lessac-high.onnx.json";
    sha256 = "0bs1j8d97v6bsvfp82h50a23kckz1scfvf312ny5gwjrk1yvjhnv";
  });

  home.file.".PowershellEditorServices".source = pkgs.fetchzip {
    url = "https://github.com/PowerShell/PowerShellEditorServices/releases/download/v3.20.0/PowerShellEditorServices.zip";
    sha256 = "XzyspX6U9FWglDA8VIZE4JamGsFvARQX7iCcQ/blbUE=";
    stripRoot = false;
  };

  # :NOTE: use image profile picture (for display manager)
  home.file.".face".source = ../../img/dracula-profile.png;
  
  home.sessionVariables = config.home.sessionVariables // {
    OPENAI_API_KEY = "$(cat ${config.sops.secrets.OPENAI_API_KEY.path})";
    GOOGLE_API_KEY = "$(cat ${config.sops.secrets.GOOGLE_API_KEY.path})";
    ANTHROPIC_API_KEY = "$(cat ${config.sops.secrets.ANTHROPIC_API_KEY.path})";
    OPENAI_API_MODEL = "gpt-5-mini";
    AIDER_MODEL = "gpt-5-mini";
    RIPGREP_CONFIG_PATH = "${my-home-dir}/.ripgreprc";
    LG_CONFIG_FILE= "${my-home-dir}/.config/lazygit/config.yml,${my-home-dir}/.config/lazygit/theme/lazygit/themes-mergable/mocha/blue.yml";
    BROWSER = "${pkgs.firefox-bin}/bin/firefox";
    TERMINAL_EMULATOR = "${pkgs.alacritty}/bin/alacritty";
    TERMINAL = "${pkgs.alacritty}/bin/alacritty";
    PAGER = "less";
    EDITOR = "nvim";
    VISUAL = "neovide";
    NIXPKGS_ALLOW_UNFREE = "1";
    MY_MACHINE_ID = "Laptop";
    GHOSTRIDER = "negative the pattern is full";
  };

}
