set nocompatible

" :NOTE: disabled this for now because caused issues with orgmode
"syntax enable

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

" Use filetype-specific indent rules, then pin JS/TS to 4 spaces.
filetype plugin indent on
autocmd FileType typescript,typescriptreact,javascript,javascriptreact setlocal shiftwidth=2 tabstop=2 softtabstop=2 expandtab

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

  -- Enable Tree-sitter highlighting and indentation for JavaScript,
  -- TypeScript, and TSX. The grammars are provided by the Nix plugin
  -- package above. nvim-treesitter's current API no longer provides
  -- nvim-treesitter.configs.
  require('nvim-treesitter').setup()
  vim.api.nvim_create_autocmd('FileType', {
    pattern = {
      'javascript',
      'javascriptreact',
      'typescript',
      'typescriptreact',
    },
    callback = function()
      vim.treesitter.start()
      vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end,
  })

  -- Setup lspconfig.
  vim.lsp.enable('pylsp')
  vim.lsp.enable('ruff')
  vim.lsp.enable('ts_ls')
  vim.lsp.enable('hls')
  vim.lsp.enable('org')
  vim.lsp.enable('csharp_ls')

  vim.api.nvim_create_autocmd('LspAttach', {
    callback = function(event)
      local opts = { buffer = event.buf, silent = true }
      vim.keymap.set('n', '<leader>lgd', vim.lsp.buf.definition, opts)
      vim.keymap.set('n', '<leader>lgD', vim.lsp.buf.declaration, opts)
      vim.keymap.set('n', '<leader>lgi', vim.lsp.buf.implementation, opts)
      vim.keymap.set('n', '<leader>lI', '<cmd>checkhealth vim.lsp<cr>', opts)
      vim.keymap.set('n', '<leader>lh', vim.lsp.buf.hover, opts)
      vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, opts)
      vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, opts)
      vim.keymap.set('n', '<leader>lR', vim.lsp.buf.references, opts)
      vim.keymap.set('n', '<leader>ls', vim.lsp.buf.signature_help, opts)
      vim.keymap.set('n', '<leader>le', vim.diagnostic.open_float, opts)
      vim.keymap.set('n', '<leader>ln', function()
        vim.diagnostic.jump({
          count = 1,
          on_jump = function() vim.diagnostic.open_float() end,
        })
      end, opts)
      vim.keymap.set('n', '<leader>lp', function()
        vim.diagnostic.jump({
          count = -1,
          on_jump = function() vim.diagnostic.open_float() end,
        })
      end, opts)
    end,
  })

  -- Setup AI codecompanion
  require("codecompanion").setup({
    strategies = {
      chat = {
        adapter = "openai",
        model = "gpt-5.6-luna"
      },
    }
  })
