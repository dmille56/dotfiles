{pkgs, ...}: 

# How to upgrade nix pkgs:
# nix-channel --update
# nix-env --upgrade
# home-manager switch --impure

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
# - install steam
# - install nix
# - install kdeconnect
# - add to /etc/default/keyboard (to remap caps lock)... might have to edit using sudo (and vi or nano).. In windows use power toys to remap caps lock
#   - XKBOPTIONS = "ctrl:nocaps" # remap caps lock to control
# - to switch Pause Break and Insert keys (for kinesis freestyle 2 keyboard)
#   - xmodmap -e "keycode 127 = Insert"
#   - xmodmap -e "keycode 118 = Pause"
# - other possible solution: edit the file pc in /usr/share/X11/xkb/symbols/
#   - ex: nano /usr/share/X11/xkb/symbols/pc
# add to .profile to fix locale issue:
# export LOCALE_ARCHIVE=$(nix-build '<nixpkgs>' -A glibcLocales)/lib/locale/locale-archive

# to fix issue: 'home-manager: line 73: NIX_PATH: unbound variable'
# export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH

# export NIX_PATH=${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels

# command to start webcam:
# cvlc v4l2:///dev/video3

# Still need to add overlays, add old notes, etc.

let
  username = "dono";
  my-dotfile-dir = "/home/${username}/dotfiles";
  my-home-dir = "/home/${username}";
in
{
  home.username = "${username}";
  home.homeDirectory = "${my-home-dir}";
  home.stateVersion = "23.11"; # To figure this out you can comment out the line and see what version it expected.
  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  home.packages = with pkgs; [
    #terminal
    wget
    nano
    kakoune
    helix
    vim
    vimgolf
    curl
    git
    ((emacsPackagesFor emacs).emacsWithPackages
      (epkgs: [ epkgs.vterm epkgs.w3m ]))
    emacs-all-the-icons-fonts
    nerdfonts
    zsh
    networkmanager
    lorri

    powershell
    tmux
    ranger
    fzf
    ripgrep

    stack
    cargo

    gnupg
    pass

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

    ollama

    tuir # rtv
    ddgr
    buku
    w3m
    yt-dlp
    ytfzf
    piper-tts
    csvtool
    ueberzugpp 
    lazygit
    gitu
    git-repo-updater #gitup

    nix-prefetch-git
    # (import ../nix/twitchy.nix) # :TODO: fix 
    (import ../nix/twitchy-rofi-script.nix)
    (import ../nix/search-ddg-script.nix)
    (import ../nix/twitchy-play-emacs.nix)
    (import ../nix/ChatGPT/ChatGPT-CLI.nix)
    (import ../nix/play-yt-script.nix)
    (import ../nix/play-yt-script-format.nix)
    (import ../nix/rofi-buku.nix)

    cmatrix
    snowmachine
    lolcat
    ormolu
    nixfmt
    libvterm
    libtool
    # rdrview

    awscli

    (haskellPackages.greenclip)
    bluez
    bluez-tools
    
    nixgl.auto.nixGLDefault # allows running OpenGL applications via nix on non nix-os systems

    #graphical

    firefox-bin
    xterm
    termonad
    alacritty
    neovide
    gparted
    chromium
    google-chrome

    mplayer
    alsaLib
    ffmpeg-full
    byzanz
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

    realvnc-vnc-viewer
    kdenlive
    flameshot

    xscreensaver
    feh

    lite-xl
    gnome3.gnome-system-monitor
    pkgs.meld
    xfce.thunar
    pcmanfm

    streamlink

    dolphinEmu
    mupen64plus
    keepassxc
    spotify

    vscode
    blueman
    darktable
    scrcpy
    redshift
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    #autosuggestion.enable = true;

    initExtra = ''
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
          rev = "c2b4aa5ad2532cca91f23908ac7f00efb7ff09c9";
          sha256 = "1b4pksrc573aklk71dn2zikiymsvq19bgvamrdffpf7azpq6kxl2";
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

    nnoremap <leader>i :Telescope current_buffer_fuzzy_find<CR>
    nnoremap <leader>us :Telescope live_grep<CR>
    nnoremap <leader>p :Telescope find_files<CR>
    nnoremap <leader>b :Telescope buffers<CR>

    nnoremap <leader>g :Neogit<CR>
    nnoremap <leader>G :LazyGit<CR>

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
      local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())
      -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
      local lspconfig = require('lspconfig')
      lspconfig.pylsp.setup { capabilities = capabilities }
      lspconfig.hls.setup {
        capabilities = capabilities,
        on_attach = function(client, bufnr)
           vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', { noremap=true, silent=true })
        end,
      }
    EOF
    '';
  };

  services.syncthing.enable = true;

  home.file.".config/kak/kakrc".text = ''
    colorscheme dracula
  '';

  home.file.".config/helix/config.toml".text = ''
    theme = "dracula"
  '';

  home.file.".config/alacritty/alacritty.toml".text = ''
    import = ["~/.config/alacritty/dracula.toml"]

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
  '';

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

  home.file.".config/kak/colors/dracula.kak".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/kakoune/master/colors/dracula.kak";
    sha256 = "57e11ca24375df2a02541a44670d4a48cf90ce179f4eecfc0f5ba005c2c03a02";
  });

  home.file.".themes/dracula".source = builtins.fetchGit {
    url = "https://github.com/dracula/gtk";
    rev = "f3c876d8c97f9bb504c98592a8d96770e70585bb";
  };
  
  home.file.".config/lazygit/theme/lazygit".source = builtins.fetchGit {
    url = "https://github.com/catppuccin/lazygit";
    rev = "a544cef9a18c3a94e0344281e0ddcf99a18a8ede";
  };

  home.file.".config/alacritty/dracula.toml".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/alacritty/master/dracula.toml";
    sha256 = "e9de3a792548c8112168c1dd18b5651d1ebee2893975cda4ccd9c4c0430c87b8";
  });

  home.sessionVariables = {
    OPENAI_API_KEY = builtins.readFile "${my-dotfile-dir}/.openai_api_key";
    # OPENAI_API_KEY = builtins.extraBuiltins.pass "OPENAI_API_KEY"; #try to get working via: https://elvishjerricco.github.io/2018/06/24/secure-declarative-key-management.html
    OPENAI_API_MODEL = "gpt-3.5-turbo"; # options: gpt-3.5-turbo, gpt-4
    RIPGREP_CONFIG_PATH = "${my-home-dir}/.ripgreprc";
    LG_CONFIG_FILE= "${my-home-dir}/.config/lazygit/config.yml,${my-home-dir}/.config/lazygit/theme/lazygit/themes-mergable/mocha/blue.yml";
    BROWSER = "sensible-browser";
    NIXPKGS_ALLOW_UNFREE = "1";
    EDITOR = "nvim";
  };

}
