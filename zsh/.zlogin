#!/bin/zsh

## ~/.zlogin

## Start an X server
#startx ${HOME}/.xinitrc bspwm > /tmp/xorg-session-log-$(date +'%F_%T') 2>&1

## Prettify linux terminal
if test $TERM = linux
then
    echo ''
fi

## EOF
