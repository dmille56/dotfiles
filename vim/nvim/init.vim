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
    
    "Neogit + dependencies
    Plug 'nvim-lua/plenary.nvim'
    Plug 'NeogitNrg/neogit'
    
    "completion
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-vsnip'
    Plug 'hrsh7th/vim-vsnip'

call plug#end()

source ~/AppData/Local/nvim/hm-init.vim