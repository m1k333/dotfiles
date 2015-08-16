#!/bin/sh

## Homespun version of suckless' dmenu_run
## Call with argument to launch in floating mode for bspwm

## Get a uniquified list of the files in $PATH
get_bins()
{
    local IFS=:
    echo "Updating cache..."
    ls ${PATH} | sort -u | sed '/^$/d' > ${CACHE}
    echo "...cache updated."
}

## Test if the launcher's cache needs to be updates
redo_cache_p()
{
    local IFS=:
    test -r ${CACHE} || return 0
    for DIR in ${PATH}
    do
        test ${DIR} -nt ${CACHE} && return 0
    done
    return 1
}

## If argument passed, bspwm will float the next window
test -n "${1}" && FLOAT=true || FLOAT=false
${FLOAT} && bspc rule -a \* -o floating=on

## Define and check/update the cache
CACHE="${XDG_CACHE_HOME:-"${HOME}/.cache"}/rofi_launcher_cache"
redo_cache_p && get_bins

## Run the launcher
DMENU_CMD="$( rofi -show -dmenu -p "run:" < ${CACHE} )"

## If a command was selected, run it
if test -n "${DMENU_CMD}"
then
    eval ${DMENU_CMD} &

## Else, if floating was enabled, disable it
elif ${FLOAT}
then
    bspc rule -r \*
fi

## EOF


