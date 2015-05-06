# ~/.zshenv

# Say `beep' again, I dare you, I double dare you!
unsetopt beep

# Make $PATH an array and exclude duplicate entries
typeset -aU path

# Add my ~/bin to $PATH, before the defaults
path=(
    $HOME/bin
    $path
)

# Set my environment
export PAGER=less
export EDITOR=vim
export BROWSER=firefox

# Let other users see (but not write) my stuff
umask 022
