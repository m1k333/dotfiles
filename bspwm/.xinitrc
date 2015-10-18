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

startawesome()
{
    # X settings
    xset -b s 1800 dpms 1800 1800 1800
    xrdb -merge ~/.config/awesome/xresources
    test -f ~/.config/awesome/xmodmap && xmodmap ~/.config/awesome/xmodmap

    # MATE DE stuff
    export DESKTOP_SESSION=gnome
    /usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &
    mate-volume-control-applet &
    mate-power-manager &
    nm-applet &

    # Run window manager
    exec awesome
}
#startbspwm
#exec mate-session
startawesome
