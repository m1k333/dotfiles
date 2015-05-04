# ~/.emacs

typeset -aU path

path=(
     $HOME/bin
     $path
     )

export PAGER=less
export EDITOR=vim
export BROWSER=firefox

umask 022
