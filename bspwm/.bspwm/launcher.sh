#!/bin/sh

## Get a uniquified list of the files in $PATH
get_bins()
{
    local IFS=:
    echo "Updating cache..."
    ls ${PATH} | sort -u | sed '/^$/d' > ${CACHE}
    echo "...cache updated."
}

## Test if the launcher's cache needs to be updated
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

## If any argument was passed, bspwm will float the next window
test ${#} -ne 0 && FLOAT=true || FLOAT=false
${FLOAT} && bspc rule -a \* -o floating=on

## Define and check/update the cache
CACHE="${XDG_CACHE_HOME:-"${HOME}/.cache"}/rofi_launcher_cache"
redo_cache_p && get_bins

## Run the launcher
ROFI_CMD="$( rofi -show -dmenu -p "run:" < ${CACHE} )"

## If a command was selected, run it
if test -n "${ROFI_CMD}"
then
    eval ${ROFI_CMD}
    exit 0

## Else, if floating was enabled, disable it
elif ${FLOAT}
then
    bspc rule -r \*
    exit 1
fi

## EOF
