#!/bin/bash

# Load default X settings
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Configure X
#setxkbmap -option ctrl:nocaps -option terminate:ctrl_alt_bksp
#xmodmap -e "keycode 135 = Super_R"
#[[ -e ${HOME}/.xmodmap ]] && xmodmap ${HOME}/.xmodmap
#xset -b 
#xset s 1800
#xset dpms 1800 1800 1800
#xrdb -merge ~/.Xresources
#[[ -e "~/.config/redshift.conf" ]] && redshift

# Make QT pick up on GTK themes and icons
#export DESKTOP_SESSION=gnome

# Default selection if no argument passed
session=${1:-bspwm}

# Launch...
case $session in
    bspwm)   (sxhkd -c ${HOME}/.bspwm/sxhkdrc &) && \
             exec bspwm -c ${HOME}/.bspwm/bspwmrc
             ;;
    stumpwm) exec stumpwm
             ;;
    *)       exec $@
             ;;
esac

# EOF
