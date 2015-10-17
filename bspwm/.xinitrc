#!/bin/sh

setxkbmap -option terminate:ctrl_alt_bksp

# merge in defaults and keymaps

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

startbspwm()
{
    BSPWMDIR="${HOME}/.config/bspwm"
    if [[ $MONITOR_SETUP == 'lab' ]] 
    then
        xrandr \
        --output HDMI2 --primary --auto \
        --output eDP1 --off
        echo 'bspc monitor HDMI2 -d 1 2 3 4 5 6 7 8 9 0' > ${BSPWMDIR}/monitors
    else
        echo 'bspc monitor -d 1 2 3 4 5 6 7 8 9 0' > ${BSPWMDIR}/monitors
    fi
    exec bspwm
}

startbspwm
