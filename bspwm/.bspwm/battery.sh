#!/bin/sh
# vim: set nowrap:

# BATTERY_LOC='NONE' disables battery monitor
BATTERY_LOC='NONE'
BATTERY_NOW="${BATTERY_LOC}/current"
BATTERY_FULL="${BATTERY_LOC}/full"
BATTERY_STATUS="${BATTERY_LOC}/status"

battery_monitor()
{
    # Should the battery monitor run?
    if test "${BATTERY_LOC}" != 'NONE'
    then
        while true
        do
            # The battery is present
            if test -e ${BAT}
            then
                # Get charging/discharging status and charge
                BPERCENT=$(echo "(${BATTERY_NOW} * 100) / ${BATTERY_FULL}" | bc)

                # Parse status
                if test "${BATTERY_STATUS}" = 'Charging'
                then
                    BSTATUS='+'
                elif test "${BATTERY_STATUS}" = 'Discharging'
                then
                    BSTATUS='-'
                else
                    BSTATUS=''
                fi

                # Parse charge
                if test ${BPERCENT} -gt 66
                then
                    BCOLOUR="%{B${green}}"
                elif test ${BPERCENT} -gt 33
                then
                    BCOLOUR="%{B${yellow}}"
                else
                    BCOLOUR="%{B${red}}"
                fi

                # The battery is not present
            else
                BCOLOUR="%{B${yellow}}"
                BSTATUS='A/C'
                BPERCENT=''
            fi

            # Output the battery status and charge, formatted for lemonbar
            echo "B%{F${black}}${BCOLOUR} ${BSTATUS}${BPERCENT} %{B-}%{F-}"
            sleep 10
        done

    # If it shouldn't run, exit
    else
        exit 1
    fi
}

##
