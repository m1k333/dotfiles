"" ~/.nvimrc

"" Settings ############################################################

" File formats
filetype plugin on
set fileformats=unix,dos,mac
autocmd FileType fortran setlocal shiftwidth=6 tabstop=6

" Indentation
filetype indent on
set shiftwidth=4 tabstop=4
set expandtab shiftround

" Input
set showcmd timeoutlen=500

" Interface
set noerrorbells novisualbell
set scrolloff=7 shortmess+=I
set statusline=./%f%m%r%h%w\ type:%Y%<%=\ %p%%\ %LL\ %04l,%04v

" Searching
set ignorecase nohlsearch smartcase

" Syntax highlighting
syntax on
set matchtime=2 showmatch

"" Functions ###########################################################

" Clean up whitespace
function! WScleanup()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunction

" Show whitespace
let g:showWS = 0
function! ToggleWS()
    if g:showWS
        set nolist
        let g:showWS = 0
    else
        set list lcs+=tab:»-,trail:·,nbsp:.
        let g:showWS = 1
    endif
endfunction

"" Keybindings #########################################################

nmap <leader>n :set invnumber<CR>
nmap <leader>ww :call ToggleWS()<CR>
nmap <leader>wc :call WScleanup()<CR>
nmap <leader>ss :w<CR>
nmap <leader>sq :wq<CR>

"" EOF #################################################################
