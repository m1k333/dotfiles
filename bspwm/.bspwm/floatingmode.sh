#!/bin/sh

bspc desktop -t floating

LAYOUT=$(bspc query -T -d focused | awk 'FNR == 2 {print $6}')
DESKTOP=$(bspc query -D -d focused | tr -d '^')
test "${LAYOUT}" = 'f' && DESKTOP="^${DESKTOP}"
bspc desktop focused -n "${DESKTOP}"

##
