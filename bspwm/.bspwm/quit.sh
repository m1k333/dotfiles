#!/bin/sh

# Send USR1 signal to start panel termination
pkill -x -SIGUSR1 launch-panel.sh

# Exit bspwm
bspc quit

## EOF
