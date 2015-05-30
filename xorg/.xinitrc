#!/bin/bash

# ^ make this the default shell for the system -- bash for arch

## Load default X settings #############################################

if [ -d /etc/X11/xinit/xinitrc.d ]
then
    for f in /etc/X11/xinit/xinitrc.d/*
    do
        [ -x $f ] && . $f
    done
    unset f
fi

## Run a window manager ################################################

# Safety first
setxkbmap -option terminate:ctrl_alt_bksp

# Run WM
SXHKD_SHELL='/bin/sh' sxhkd -c ${HOME}/.bspwm/sxhkdrc &
exec bspwm -c ${HOME}/.bspwm/bspwmrc

## EOF #################################################################
