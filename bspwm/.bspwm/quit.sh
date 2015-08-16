#!/bin/sh

. ${HOME}/.bspwm/setup-panel.sh

rm -Rf $PANEL_FIFO

for ARG in launch-panel.sh lemonbar xtitle bspc
do
    while pgrep -x $ARG > /dev/null 2>&1
    do
        pkill -x -9 $ARG > /dev/null 2>&1
    done
done

bspc quit

## EOF
