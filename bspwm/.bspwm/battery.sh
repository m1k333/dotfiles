#!/bin/sh
# vim: set nowrap:

# BATTERY_LOC='NONE' disables battery monitor
BATTERY_LOC='/sys/class/power_supply/BAT1'
BATTERY_NOW="${BATTERY_LOC}/energy_now"
BATTERY_FULL="${BATTERY_LOC}/energy_full"
BATTERY_STATUS="${BATTERY_LOC}/status"

. ${HOME}/.bspwm/panelrc

battery_monitor()
{
    if test "${BATTERY_LOC}" != 'NONE'
    then
        while true
        do
            STATUS="$(acpi -b | awk '{print $3" "$4}' | tr -d ',')"
            test -z "${STATUS}" && STATUS='AC'
            echo "B[${STATUS}]"
            sleep 5
        done
    fi
}

##
