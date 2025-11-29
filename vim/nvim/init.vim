" Remember to call :PlugInstall and :PlugUpdate to update
" Remember to install vim-plug: https://github.com/junegunn/vim-plug
" Note: airline-themes needs 'base16-dracula' for non nix install instead of 'dracula' for some reason

"Vim-plug
call plug#begin('~/AppData/Local/nvim/plugged')

    Plug 'LnL7/vim-nix'
    Plug 'liuchengxu/vim-which-key'
    Plug 'Mofiqul/dracula.nvim'
    Plug 'preservim/nerdtree'
    Plug 'kien/ctrlp.vim'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'neovim/nvim-lspconfig'
    Plug 'easymotion/vim-easymotion'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'jvgrootveld/telescope-zoxide'
    Plug 'roman/golden-ratio'
    Plug 'kdheepak/lazygit.nvim'
    Plug 'nvim-orgmode/orgmode'
    
    "Neogit + dependencies
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'NeogitOrg/neogit'
    
    "completion
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-vsnip'
    Plug 'hrsh7th/vim-vsnip'

    "AI Codecompanion + dependencies
    " :NOTE: The plugin requires the markdown and markdown_inline Tree-sitter parsers to be installed with :TSInstall markdown markdown_inline
    Plug 'nvim-lua/plenary.nvim'
    Plug 'olimorris/codecompanion'

call plug#end()

source ~/AppData/Local/nvim/hm-init.vim