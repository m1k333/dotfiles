#!/bin/sh

setxkbmap -option terminate:ctrl_alt_bksp
test -e ${HOME}/.xmodmap && xmodmap ${HOME}/.xmodmap
xrdb -I${HOME} ${HOME}/.Xresources.d/main

##
