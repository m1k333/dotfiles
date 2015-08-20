#!/bin/sh

# Send term signals to panel.sh
pkill -x panel.sh

# Remove temporary files
rm -Rf /tmp/bspwm

# Exit bspwm
bspc quit

## EOF
