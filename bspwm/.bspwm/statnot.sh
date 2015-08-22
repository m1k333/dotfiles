#!/bin/sh

if test ${#} -eq 0 
then
    #echo "N$(whoami)@$(hostname)" > /tmp/bspwm/panel_fifo 
    echo "N$(cat /proc/loadavg | awk '{print $1","$2","$3}') $(free -m | awk 'FNR == 2 {print $3}')MiB" > /tmp/bspwm/panel_fifo 
else
    eval $(grep 'red=' ${HOME}/.bspwm/colors)
    echo "${1}" >> ${HOME}/.notification.log

    # .. Lemonbar colour formatting ........ Remove markdown ..... #
    echo "N%{F#FF${red}}${1}%{F-}" | sed -e 's/<[^>]*>//g' \
        > /tmp/bspwm/panel_fifo 
fi

##
