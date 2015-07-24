#!/bin/bash

# ^ make this the default shell for the system -- bash for arch

## Load default X settings #############################################

# Load system settings (pull this from the system's default xinitrc)
if [ -d /etc/X11/xinit/xinitrc.d ]
then
    for f in /etc/X11/xinit/xinitrc.d/*
    do
        [ -x $f ] && . $f
    done
    unset f
fi

# Always be able to safely terminate X
setxkbmap -option terminate:ctrl_alt_bksp

## Run a window manager ################################################

WM=bspwm

case $WM in
    bspwm)
        SXHKD_SHELL='/bin/sh' sxhkd -c ${HOME}/.bspwm/sxhkdrc &
        exec bspwm -c ${HOME}/.bspwm/bspwmrc
        ;;
    xfce)
        exec xfce4-session
        ;;
    *)
        exec $WM
        ;;
esac

## EOF #################################################################
