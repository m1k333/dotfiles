## ~/.zprofile

## Add my ~/bin to $PATH, before the defaults
typeset -aU path # Disallow duplicate entries
path=($HOME/bin $path)
export PATH

## Set my environment
export PAGER=less
export EDITOR=vim
export BROWSER=firefox

## Let other users see (but not write) my stuff
umask 022
