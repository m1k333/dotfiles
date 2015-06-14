#!/bin/zsh

## ~/.zlogin ###########################################################

## Start an X server ###################################################

    #####################
        STARTX=true
    #####################
    
if ${STARTX}; then 
    [ -z $DISPLAY -a $XDG_VTNR -eq 1 ] && \
    startx -- vt$XDG_VTNR > /tmp/xorg-session-log-$(date +'%F_%T') 2>&1
elif [ $TERM = linux ]; then
    echo ''
fi

## EOF #################################################################
