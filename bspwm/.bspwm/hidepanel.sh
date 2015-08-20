#!/bin/sh

. ${HOME}/.bspwm/panelrc

WID=$(cat /tmp/bspwm/panel_wid)

if wattr m ${WID}
then
    bspc config top_padding 0
    mapw -u ${WID}
else
    bspc config top_padding ${PANEL_HEIGHT}
    mapw -m ${WID}
fi

##

