#!/bin/sh

if xdotool search --onlyvisible --name 'bar' windowunmap
then
    bspc config top_padding 0
else
    xdotool search --name 'bar' windowmap   
    eval $(grep 'PANEL_HEIGHT=' ${HOME}/.bspwm/panelrc)
    bspc config top_padding ${PANEL_HEIGHT}
fi

##

