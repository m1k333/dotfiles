#!/bin/sh

# Load default X settings

if [ -d /etc/X11/xinit/xinitrc.d ]
then
    for f in /etc/X11/xinit/xinitrc.d/*
    do
        [ -x $f ] && . $f
    done
    unset f
fi

# Safety first
setxkbmap -option terminate:ctrl_alt_bksp

# Run WM
SXHKD_SHELL='/bin/dash' sxhkd -c ${HOME}/.bspwm/sxhkdrc &
exec bspwm -c ${HOME}/.bspwm/bspwmrc

# EOF
