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

# export NIX_PATH=${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels

let
  my-dotfile-dir = "/home/dono/dotfiles";
  my-home-dir = "/home/dono";
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  nixpkgs.overlays = [
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay.git";
      rev = "e962b871a0b0984569506a576543eff8926d478f";
    }))

    (self: super: {
      mpv = super.mpv.override {
       scripts = [ self.mpvScripts.youtube-quality ];
      };
    })
  ];

  home.packages = with pkgs; [

    #terminal

    wget
    vim
    curl
    git
    # ((emacsPackagesFor emacsGit).emacsWithPackages # for emacs overlay
    ((emacsPackagesFor emacs).emacsWithPackages
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
    bottom
    delta

    cmus
    pandoc

    xclip
    powerline-fonts

    tuir # rtv
    ddgr
    w3m
    youtube-dl
    yt-dlp

    youtube-viewer
    urlview

    nix-prefetch-git
    dropbox
    # (import ../nix/twitchy.nix) # TODO: fix 
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
    keepassxc

    vscode
    protontricks

    blueman

    darktable

    # android-studio
    scrcpy
    flameshot

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

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    delta = {
      enable = true;
      options = {
        features = "dracula";
      };
    };
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

      # completion
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      nvim-cmp
      cmp-vsnip
      vim-vsnip
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

    let mapleader="\<SPACE>"

    "NERDTree
    "-------------------------
    nnoremap <leader>\ :NERDTreeToggle<CR>
    let NERDTreeIgnore = [ '\.js_dyn_o', '\.js_hi', '\.js_o', '\.js_dyn_hi', '\.dyn_hi', '\.dyn_o', '\.hi', '\.o', '\.p_hi', '\.p_o' ]
    "Automatically close if NERDTree is the only buffer left
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

    " Open file menu
    nnoremap <Leader>o :CtrlP<CR>
    " Open buffer menu
    nnoremap <Leader>b :CtrlPBuffer<CR>
    " Open most recently used files
    nnoremap <Leader>f :CtrlPMRUFiles<CR>

    nnoremap <Leader>a :tabprev<CR>
    nnoremap <Leader>s :tabnext<CR>

    let g:airline_powerline_fonts = 1
    let g:airline_theme= 'dracula'

    set completeopt=menu,menuone,noselect

    lua <<EOF
      -- Setup nvim-cmp.
      local cmp = require'cmp'

      cmp.setup {
          mapping = {
              ["<C-d>"] = cmp.mapping.scroll_docs(-4),
              ["<C-f>"] = cmp.mapping.scroll_docs(4),
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
      local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())
      -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
      require('lspconfig')['hls'].setup {
        capabilities = capabilities,
        on_attach = function(client, bufnr)
           vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', { noremap=true, silent=true })
        end,
      }
    EOF
    '';
  };

  programs.home-manager = { enable = true; };

  home.file.".vimrc".source = "${my-dotfile-dir}/vimrc";
  home.file.".Xresources".source = "${my-dotfile-dir}/Xresources";
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  home.file.".xmonad/xmonad.hs".source = "${my-dotfile-dir}/.xmonad/xmonad.hs";
  home.file.".ghci".source = "${my-dotfile-dir}/.ghci";
  home.file.".ripgreprc".source = "${my-dotfile-dir}/.ripgreprc";
  home.file.".config/mpv/mpv.conf".source = "${my-dotfile-dir}/mpv.conf";

  manual.manpages.enable = false; # TODO: reenable man pages eventually... they wouldn't update correctly
}
