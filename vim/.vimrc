"" ~/.vimrc MSR 2014 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"" 'diet' version, with 110% less cruft! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"" Appearance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

" Bell (off)
set noerrorbells
set novisualbell
set timeoutlen=500
set t_vb=

" Colourscheme
colorscheme elflord

" Scroll buffer
set scrolloff=7

" Splash screen
set shortmess=atI


" Status line
set laststatus=2
set statusline=./%f%m%r%h%w\ type:%Y%<%=\ %p%%\ (%LL)\ (%04l,%04v)

"" Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

" Buffers update when file is changed
set autoread

" Encoding
set encoding=utf-8
set ffs=unix,dos,mac

" Filetype syntax HL and indentation
filetype on
filetype plugin on
filetype indent on
syntax on
set showmatch
set mat=2

" New files default to text mode
autocmd BufNewFile,BufRead * setfiletype text

" Specific filetype options
autocmd FileType fortran setlocal tabstop=6 shiftwidth=6

"" Searching ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set incsearch
set ignorecase
set smartcase
set magic
set wildmenu

"" Tabs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set smarttab
set expandtab
set shiftround
set tabstop=4
set shiftwidth=4

"" Command to clean up whitespace ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
func! WScleanup()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunc
command! WScleanup call WScleanup()
nmap <leader>w :WScleanup<CR>

"" Wrap ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set backspace=indent,eol,start
set textwidth=80
set formatoptions+=t

"" EOF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
