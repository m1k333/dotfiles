#!/bin/zsh

## ~/.zlogin ###########################################################

## Start an X server ###################################################

[ -z $DISPLAY -a $XDG_VTNR -eq 1 ] && \
startx -- vt$XDG_VTNR > /tmp/xorg-session-log-$(date +'%F_%T') 2>&1

## Start a linux console ###############################################

[ $TERM = linux ] && echo '' # New line for prettiness

## EOF #################################################################
