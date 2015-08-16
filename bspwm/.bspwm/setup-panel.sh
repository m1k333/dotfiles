#!/bin/sh

PANEL_FONT='DejaVu Sans Mono:bold:size=10'
PANEL_HEIGHT=16
PANEL_FIFO='/tmp/bspwm-panel-fifo'
PANEL_MAPPED_NAME='bspwmpanelmapped'
PANEL_UNMAPPED_NAME='bspwmpanelunmapped'
BATTERY_LOC='NONE' # BATTERY='NONE' to disable bat. monitor
BATTERY_NOW="${BATTERY_LOC}/current"
BATTERY_FULL="${BATTERY_LOC}/full"
BATTERY_STATUS="${BATTERY_LOC}/status"
foreground='#FF93a1a1'
background='#FF000000'
black='#FF000000'
blue='#FF268bd2'
cyan='#FF2aa198'
green='#FF859900'
magenta='#FFd33682'
orange='#FFcb4b16'
red='#FFdc322f'
violet='#FF6c71c4'
white='#FFB0B0B0'
yellow='#FFb58900'

if [ ! -z $1 ]
then
    if xdotool search --name $PANEL_MAPPED_NAME
    then
        xdotool search --name $PANEL_MAPPED_NAME windowunmap
        bspc config top_padding 0
        xdotool search --sync --name $PANEL_MAPPED_NAME set_window --name $PANEL_UNMAPPED_NAME
    elif xdotool search --name $PANEL_UNMAPPED_NAME
    then
        xdotool search --name $PANEL_UNMAPPED_NAME windowmap
        bspc config top_padding $PANEL_HEIGHT
        xdotool search --sync --name $PANEL_UNMAPPED_NAME set_window --name $PANEL_MAPPED_NAME
    fi
fi

## EOF
