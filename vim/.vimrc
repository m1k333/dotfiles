"
"Options
"

"Appearance
if has ("gui_running")
   set guiheadroom=0
   set guioptions-=T
else
   set bg=dark
endif

"Auto-Read
set autoread

"Beeping (off!!!)
set noerrorbells
set novisualbell
set t_vb=
set tm=500

"Encoding
set encoding=utf-8

"Filetype functionality
filetype on
filetype plugin on
filetype indent on
autocmd BufNewFile,BufRead * setfiletype text

"Filetype-specific options
autocmd FileType text setlocal wrap
autocmd FileType fortran setlocal tabstop=6 shiftwidth=6

"Line and column numbers
set ruler
set number

"Macros
set lazyredraw

"Mouse
set mouse=a

"Newlines
set ffs=unix,dos,mac

"Scrolloff
set scrolloff=7

"Search
set incsearch
set smartcase
set ignorecase
set magic

"Spell-checking
set spelllang=en_CA

"Startup
set shortmess=atI

"Status Line
set laststatus=2
set statusline =./%f%m%r%h%w\ type:%Y%<%=\ %p%%\ (%LL)\ (%04l,%04v)

"Syntax highlighting
syntax on
set showmatch
set mat=2

"Tabs and whitespace
set list
exec "set listchars=tab:\uBB\uBB,trail:\uB7,nbsp:~"
set smarttab
set expandtab
set shiftround
set tabstop=4
set shiftwidth=4

"Tab-completion
set wildmenu

"Wrap
set backspace=indent,eol,start
set whichwrap=b,s,h,l,<,>,~,[,]
set wrapmargin=0
set nowrap
set linebreak
set textwidth=0

"
"Functions
"

"Delete trailing whitespace
func! DeleteTrailingWS()
      exe "normal mz"
      %s/\s\+$//ge
      exe "normal `z"
endfunc

"
"Keybindings
"

"Move up and down visually
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

"Quick 'replace all'
nmap <leader>g :%s//g<LEFT><LEFT>
vmap <leader>g :s//g<LEFT><LEFT>

"Toggle between absolute and relative numbering
nmap <leader>n :set invrelativenumber<CR>

"Retab
nmap <leader>t :retab<CR>

"Remove trailing whitespace
nmap <leader>w :call DeleteTrailingWS()<CR>
