#!/bin/sh

# Remove extant rules
bspc rule -r \*

# If argument passed, float next window
test ! -z $1 && bspc rule -a \* -o floating=on

# Launcher
rofi -show run

## EOF
