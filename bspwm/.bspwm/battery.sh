#!/bin/sh
# vim: set nowrap:

# BATTERY_LOC='NONE' disables battery monitor
BATTERY_LOC='NONE'
BATTERY_NOW="${BATTERY_LOC}/current"
BATTERY_FULL="${BATTERY_LOC}/full"
BATTERY_STATUS="${BATTERY_LOC}/status"

. ${HOME}/.bspwm/panelrc

battery_monitor()
{
    if test "${BATTERY_LOC}" != 'NONE'
    then

        while true
        do
            if test -e ${BAT}
            then
                BPERCENT=$(echo "(${BATTERY_NOW} * 100) / ${BATTERY_FULL}" | bc)
                if test "${BATTERY_STATUS}" = 'Charging'
                then
                    BSTATUS='+'
                elif test "${BATTERY_STATUS}" = 'Discharging'
                then
                    BSTATUS='-'
                else
                    BSTATUS=''
                fi

                if test ${BPERCENT} -gt 66
                then
                    BCOLOUR="%{F${green}}"
                elif test ${BPERCENT} -gt 33
                then
                    BCOLOUR="%{F${yellow}}"
                else
                    BCOLOUR="%{F${red}}"
                fi
            else
                BCOLOUR="%{B${yellow}}"
                BSTATUS='AC'
                BPERCENT=''
            fi
            echo "B${BCOLOUR}${BSTATUS}${BPERCENT}%{F-}"
            sleep 10
        done

    else
        echo "BAC"
        exit 1
    fi
}

##
