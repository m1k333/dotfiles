#!/bin/sh

DESKTOP="$(bspc query -D -d focused | tr -d '^!')"

FLOATINGP=$(bspc query -T -d focused | awk 'FNR == 2 {print $6}')
test "${FLOATINGP}" = 'f' && bspc desktop -t floating

MONOCLEP=$(bspc query -T -d focused | awk 'FNR == 2 {print $5}')
test "${MONOCLEP}" = 'M' || DESKTOP="!${DESKTOP}"

bspc desktop -l next
bspc desktop focused -n "${DESKTOP}"

##
