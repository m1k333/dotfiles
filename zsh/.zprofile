# ~/.zprofile

# Say `beep' again, I dare you, I double dare you!
# One of these will hopefully kill the bell.  Linux sucks.
unsetopt beep
set bell-style none
[[ "$TERM" == 'linux' ]] && setterm -blength 0

# Make $path an array and exclude duplicate entries
typeset -aU path

# Add my ~/bin to $PATH, before the defaults
path=($HOME/bin $path)
export PATH

# Set my environment
export PAGER=less
export EDITOR=vim
export BROWSER=firefox

# Let other users see (but not write) my stuff
umask 022
