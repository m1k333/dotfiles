#!/bin/sh

## X settings
test -e ${HOME}/.xmodmap && xmodmap ${HOME}/.xmodmap
xset -b s 1800 dpms 1800 1800 1800
xrdb -load ${HOME}/.Xresources

## Dock, other daemons
pgrep mpd || mpd &

## EOF
