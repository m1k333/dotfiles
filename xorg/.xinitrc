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

# X settings
test -e ${HOME}/.xmodmap && xmodmap ${HOME}/.xmodmap
xset -b s 1800 dpms 1800 1800 1800
xrdb -I${HOME} ${HOME}/.Xresources.d/main
xsetroot -solid black -cursor_name left_ptr
export DESKTOP_SESSION=gnome # Make QT use GTK settings

# External applications
mpd &

# Window manager
exec bspwm -c ${HOME}/.bspwm/bspwmrc

## EOF

