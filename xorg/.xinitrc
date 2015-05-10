#!/bin/bash

## Load default X settings
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

## Configure X
setxkbmap -option ctrl:nocaps -option terminate:ctrl_alt_bksp
[ -f ${HOME}/.xmodmap ] && xmodmap ${HOME}/.xmodmap
xset -b s 3600 dpms 7200 7200 7200

## Run window manager

# Default selection if no argument passed
session=${1:-bspwm}

# Launch...
case $session in
    bspwm)   (sxhkd -c ${HOME}/.bspwm/sxhkdrc &) && \
             exec bspwm -c ${HOME}/.bspwm/bspwmrc
             ;;
    emacs)   exec emacs --eval "(toggle-frame-fullscreen)"
             ;;
    stumpwm) exec stumpwm
             ;;
    *)       exec $@
            ;;
esac

## EOF
