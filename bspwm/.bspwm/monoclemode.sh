#!/bin/sh

bspc desktop -l next

LAYOUT=$(bspc query -T -d focused | awk 'FNR == 2 {print $5}')
DESKTOP=$(bspc query -D -d focused | tr -d '+')
test "${LAYOUT}" = 'M' && DESKTOP="${DESKTOP}+"
bspc desktop focused -n "${DESKTOP}"

##
