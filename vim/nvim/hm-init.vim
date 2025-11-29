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

" Use system clipboard for all yank/delete/put operations
set clipboard+=unnamedplus

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

nnoremap gb <C-w>w<CR>

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
