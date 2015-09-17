#!/bin/bash

# ^ make this the default shell for the system -- bash for arch

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

# Multihead
if test ${#} -ne 0
then
    xrandr --output LVDS --auto --right-of VGA-0 --output VGA-0 --primary --auto
    exec bspwm -c ${HOME}/.bspwm/bspwmrc-multihead
else 

# Window manager
    exec bspwm -c ${HOME}/.bspwm/bspwmrc
fi

## EOF