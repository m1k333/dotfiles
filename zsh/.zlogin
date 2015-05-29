#!/bin/zsh

## ~/.zlogin

## Start an X server
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx -- vt$XDG_VTNR

## Start a linux console
[ $TERM = linux ] && echo '' # New line for prettiness

## EOF
