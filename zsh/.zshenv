#!/bin/zsh

# Add ${HOME}/bin to ${PATH}, before the system-wide defaults
typeset -aU path
path=(${HOME}/bin ${HOME}/.local/bin ${path})
export PATH

# Set up environment
export BROWSER=firefox
export EDITOR=vim
export PAGER=less
export XAUTHORITY=${HOME}/.Xauthority
ulimit -c 0
umask 022

## EOF
