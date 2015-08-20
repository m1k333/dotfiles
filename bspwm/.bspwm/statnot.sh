#!/bin/sh

if test ${#} -eq 0 
then
    echo "N$(whoami)@$(hostname)" > /tmp/bspwm/panel_fifo 
else
    eval $(grep 'red=' ${HOME}/.bspwm/colors)
    echo "N%{F#FF${red}}Notice: ${1}%{F-}" > /tmp/bspwm/panel_fifo 
    echo "${1}" >> ${HOME}/.notification.log
fi

##
