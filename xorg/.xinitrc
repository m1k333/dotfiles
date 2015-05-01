#!/bin/sh

#Load defaults
if [ -d /etc/X11/xinit/xinitrc.d ]; then
   for f in /etc/X11/xinit/xinitrc.d/*; do
       [ -x "$f" ] && . "$f"
   done
   unset f
fi

#Configure X
setxkbmap -option ctrl:nocaps -option terminate:ctrl_alt_bksp
xset -b s 3600 dpms 7200 7200 7200

#Run window manager
(sxhkd -c ~/.config/bspwm/sxhkdrc &) && exec bspwm
