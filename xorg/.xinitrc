#!/bin/bash

# Load default X settings

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Safety first
setxkbmap -option terminate:ctrl_alt_bksp

# Run WM
sxhkd -c ${HOME}/.bspwm/sxhkdrc &
exec bspwm -c ${HOME}/.bspwm/bspwmrc

# EOF
