#!/bin/sh

if test ${#} -eq 0 
then
    echo "N$(whoami)@$(hostname)" > /tmp/bspwm/panel_fifo 
else
    eval $(grep 'red=' ${HOME}/.bspwm/colors)
    echo "${1}" >> ${HOME}/.notification.log

    # .. Lemonbar colour formatting ........ Remove markdown ..... #
    echo "N%{F#FF${red}}Notice: ${1}%{F-}" | sed -e 's/<[^>]*>//g' \
        > /tmp/bspwm/panel_fifo 
fi

##
