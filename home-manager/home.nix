{ pkgs, ... }:

# How to upgrade nix pkgs:
# nix-channel --update
# nix-env --upgrade
# home-manager switch

# Upgrade ubuntu:
# sudo apt-get update
# sudo apt-get upgrade

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
# - add to /etc/default/keyboard (to remap caps lock)... might have to edit using sudo (and vi or nano).. In windows use power toys to remap caps lock
#   - XKBOPTIONS = "ctrl:nocaps" # remap caps lock to control
# add to .profile to fix locale issue:
# export LOCALE_ARCHIVE=$(nix-build '<nixpkgs>' -A glibcLocales)/lib/locale/locale-archive

# to fix issue: 'home-manager: line 73: NIX_PATH: unbound variable'
# export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH

# export NIX_PATH=${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels

# command to start webcam:
# cvlc v4l2:///dev/video3

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
       scripts = [ self.mpvScripts.quality-menu ];
      };

      realvnc-vnc-viewer = super.realvnc-vnc-viewer.overrideAttrs (old: {
        version = "7.1.0";
        src = {
          "x86_64-linux" = super.fetchurl {
            url = "https://downloads.realvnc.com/download/file/viewer.files/VNC-Viewer-7.1.0-Linux-x64.rpm";
            sha256 = "327e0ad872022bba301dc5c7f39209527727a4f679eb33726e41d9362a989076";
          };
        }.${super.stdenv.system} or (throw "Unsupported system: ${super.stdenv.hostPlatform.system}");
        buildInputs = old.buildInputs ++ [
          super.stdenv.cc.cc.libgcc
        ];
      });

   })
  ];

  home.packages = with pkgs; [

    #terminal

    wget
    nano
    kakoune
    vim
    vimgolf
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

    powershell

    tmux
    zellij
    ranger
    fzf
    ripgrep

    stack
    cargo
    sbcl

    gnupg
    pass

    bat
    eza #exa
    fd
    bottom
    delta
    thefuck

    cmus
    pandoc

    xclip
    powerline-fonts

    warpd

    tuir # rtv
    ddgr
    w3m
    yt-dlp
    ytfzf
    csvtool
    ueberzugpp 
    gitui

    nix-prefetch-git
    dropbox
    # (import ../nix/twitchy.nix) # :TODO: fix 
    (import ../nix/twitchy-rofi-script.nix)
    (import ../nix/search-ddg-script.nix)
    (import ../nix/twitchy-play-emacs.nix)
    (import ../nix/ChatGPT/ChatGPT-CLI.nix)
    (import ../nix/play-yt-script.nix)
    (import ../nix/play-yt-script-format.nix)

    cmatrix
    snowmachine
    lolcat
    ormolu
    nixfmt
    rnix-lsp
    closurecompiler
    nodejs
    cmake
    libvterm
    libtool
    # rdrview

    awscli

    (haskellPackages.greenclip)
    bluez
    bluez-tools

    #graphical

    # kdeconnect

    firefox-bin
    xterm
    termonad
    gparted
    chromium
    google-chrome

    mplayer
    alsaLib
    ffmpeg-full
    # mpv
    vlc
    pavucontrol
    pasystray
    playerctl

    # rofi
    rofi-bluetooth 
    xdotool
    noto-fonts-color-emoji

    gmrun
    dmenu
    conky

    guvcview
    realvnc-vnc-viewer

    xscreensaver
    feh

    lite-xl
    gedit
    gnome3.gnome-system-monitor
    pkgs.meld
    xfce.thunar
    pcmanfm
    dolphin

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

  programs.thefuck.enable = true;

  programs.rofi = {
    enable = true;
    plugins = [pkgs.rofi-emoji];
    # theme = "glue_pro_blue"; #good fallback theme that comes installed with rofi
    theme = "dracula-theme";
  };

  programs.git = {
    enable = true;
    delta = {
      enable = true;
      options = {
        features = "dracula";
      };
    };
    aliases = {
      st = "status";
      ci = "commit"; 
      br = "branch";
      co = "checkout";
    };
    extraConfig = {
      color = {
        ui = "always";
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
      vim-easymotion
      telescope-nvim
      telescope-zoxide
      neogit
      golden-ratio

      # completion
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      nvim-cmp
      cmp-vsnip
      vim-vsnip

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
    map  <Leader>j <Plug>(easymotion-bd-f)
    nmap <Leader>j <Plug>(easymotion-overwin-f)

    " Move to line
    map <Leader>f <Plug>(easymotion-bd-jk)
    nmap <Leader>f <Plug>(easymotion-overwin-line)

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

    nnoremap <leader>i :Telescope live_grep<CR>
    nnoremap <leader>p :Telescope find_files<CR>
    nnoremap <leader>b :Telescope buffers<CR>

    nnoremap <leader>g :Neogit<CR>

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

  home.file.".vimrc".source = "${my-dotfile-dir}/vim/vim/vimrc";
  home.file.".Xresources".source = "${my-dotfile-dir}/Xresources";
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  home.file.".xmonad/xmonad.hs".source = "${my-dotfile-dir}/.xmonad/xmonad.hs";
  home.file.".xmonad/lib/MyTheme.hs".source = "${my-dotfile-dir}/.xmonad/MyTheme.hs";
  home.file.".ghci".source = "${my-dotfile-dir}/.ghci";
  home.file.".ripgreprc".source = "${my-dotfile-dir}/.ripgreprc";
  home.file.".config/mpv/mpv.conf".source = "${my-dotfile-dir}/mpv.conf";
  home.file.".config/ytfzf/conf.sh".source = "${my-dotfile-dir}/ytfzf-conf.sh";
  home.file.".config/termonad/termonad.hs".source = "${my-dotfile-dir}/.termonad/termonad.hs";
 
  home.file.".config/rofi/themes/dracula-theme.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/rofi/48a024639fbf25e3237766f0dcef4af75a2df908/theme/config1.rasi";
    sha256 = "52f26dd7c44bb919a7a604d71bea26df5e52bd2188f9804e103fc002239bc99a";
  });

  home.file.".config/rofi/themes/dracula-theme-2.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/rofi/48a024639fbf25e3237766f0dcef4af75a2df908/theme/config2.rasi";
    sha256 = "68010556ad7b351b63b6d061f5c4b7c8feb9d9b32687bf0530b105a86634766c";
  });

  home.file.".themes/dracula".source = builtins.fetchGit {
    url = "https://github.com/dracula/gtk";
    rev = "f3c876d8c97f9bb504c98592a8d96770e70585bb";
  };

  home.stateVersion = "18.09"; # one of "18.09", "19.03", "19.09", "20.03", "20.09", "21.03", "21.05", "21.11", "22.05", "22.11", "23.05", "23.11"

  fonts.fontconfig.enable = true;
  
  home.sessionVariables = {
    OPENAI_API_KEY = builtins.readFile "${my-dotfile-dir}/.openai_api_key";
    # OPENAI_API_KEY = builtins.extraBuiltins.pass "OPENAI_API_KEY"; #try to get working via: https://elvishjerricco.github.io/2018/06/24/secure-declarative-key-management.html
    OPENAI_API_MODEL = "gpt-3.5-turbo"; # options: gpt-3.5-turbo, gpt-4
    RIPGREP_CONFIG_PATH = "${my-home-dir}/.ripgreprc";
  };

  manual.manpages.enable = false; # :TODO: reenable man pages eventually... they wouldn't update correctly
}
