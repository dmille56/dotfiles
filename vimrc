set nocompatible

if has("syntax")
  syntax on
endif

set background=dark

if has("autocmd")
  filetype plugin indent on
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
"set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
"set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
"set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
"set hidden		" Hide buffers when they are abandoned
set mouse=a		" Enable mouse usage (all modes)

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

set autochdir
map - :Texplore <Return>
map a :tabprev <Return>
map s :tabnext <Return>

"remap ; to : to save a keystroke
nnoremap : ;
nnoremap ; :
vnoremap : ;
vnoremap ; :