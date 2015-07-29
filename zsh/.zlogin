#!/bin/zsh

## ~/.zlogin

## Start an X server (Broken after ZSH update?!?!?!?)
#startx -- vt$XDG_VTNR &> /tmp/xorg-session-log-$(date +'%F_%T')

## Prettify linux terminal
if test $TERM = linux
then
    echo ''
fi

## EOF
