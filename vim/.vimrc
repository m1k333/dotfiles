"" #/.vimrc MSR 2014 ###################################################
"" 'diet' version, with 110% less cruft! ###############################

"" Appearance ##########################################################

" Visuals
set noerrorbells novisualbell t_vb=
if has("gui_running")
    set guioptions= guiheadroom=0
    colorscheme solarized
else
    set background=light
    colorscheme desert
endif

" Information
set laststatus=2 modeline shortmess=atI showcmd 
set statusline=./%f%m%r%h%w\ type:%Y%<%=\ %p%%\ (%LL)\ (%04l,%04v)

"" Files ###############################################################

" Buffers update when file is changed
set autoread

" Encoding
set encoding=utf-8 fileformats=unix,dos,mac

" Filetype syntax HL and indentation
filetype plugin indent on
syntax on
set showmatch matchtime=2 
set formatoptions=c,q,r,t

" New files default to text mode
autocmd BufNewFile,BufRead * setfiletype text

" Specific filetype options
autocmd FileType fortran setlocal shiftwidth=6 tabstop=6 

"" Functions and commands ##############################################

" Clean up whitespace
func! WScleanup()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunc
command! WScleanup call WScleanup()

"" Keybindings #########################################################
set scrolloff=7
set timeoutlen=500
nmap <leader>n :set invnumber<CR>
nmap <leader>w :WScleanup<CR>

"" Searching ###########################################################
set incsearch 
set ignorecase smartcase

"" Tabs ################################################################

" Indentation
set autoindent smartindent
set expandtab shiftround smarttab 
set shiftwidth=4 tabstop=4

" Tab-completion
set wildmenu wildmode=list:longest,full

"" Wrap ################################################################
set backspace=indent,eol,start 
set textwidth=80

"" EOF #################################################################
