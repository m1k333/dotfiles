#!/bin/sh

xset -b s 1800 dpms 1800 1800 1800
test -e ${HOME}/.xmodmap && xmodmap ${HOME}/.xmodmap
xrdb -load ${HOME}/.Xresources

nm-applet &
mpc status || mpd &
transmission-gtk -m &

## EOF
