#!/bin/sh

nvidia-settings -l
test -e ${HOME}/.xmodmap && xmodmap ${HOME}/.xmodmap
xrdb -I${HOME} ${HOME}/.Xresources.d/main

#
