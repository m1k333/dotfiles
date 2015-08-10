#!/bin/zsh

## ~/.zprofile #########################################################

## Add my ~/bin to $PATH, before the defaults ##########################

typeset -aU path            # Disallow duplicate entries in $path
path=(${HOME}/bin ${path})  # Add ~/bin, $PATH to the $path array
export PATH                 # Export $PATH; zsh knows to use $path

## Set my environment ##################################################

export BROWSER=firefox
export EDITOR=vim
export PAGER=less
export XAUTHORITY=${HOME}/.Xauthority
ulimit -c 0
umask 022

## EOF #################################################################
