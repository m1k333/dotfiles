#!/bin/sh

DESKTOP="$(bspc query -D -d focused | tr -d '^!')"

MONOCLEP=$(bspc query -T -d focused | awk 'FNR == 2 {print $5}')
test "${MONOCLEP}" = 'M' && bspc desktop -l next

FLOATINGP=$(bspc query -T -d focused | awk 'FNR == 2 {print $6}')
test "${FLOATINGP}" = 'f' || DESKTOP="^${DESKTOP}"

bspc desktop -t floating
bspc desktop focused -n "${DESKTOP}"

##
