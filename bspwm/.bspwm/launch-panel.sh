#!/bin/sh
# vim: set nowrap:

###
### SECTION: Setup 
###

## Panel variables
. ${HOME}/.bspwm/setup-panel.sh && source_panel

##
## SECTION: Cleanup
##

## Clean function
panel_clean()
{
    # Clean up older panels (keeping this instance)
    PROC_COUNT=$(pgrep -cx launch-panel.sh)
    while test ${PROC_COUNT} -gt 1
    do
        pkill -ox launch-panel.sh
        PROC_COUNT=$(pgrep -cx launch-panel.sh)
    done
    unset PROC_COUNT

    # Clean up orphaned programs from dead panels
    for ARG in bspc xtitle lemonbar
    do
        while pgrep -x ${ARG}
        do
            pkill -x ${ARG}
        done
    done

    # Remove padding bspc padding
    bspc config top_padding 0

    # Remove old FIFOs
    rm -f ${PANEL_FIFO}
}

## Trap signals; terminate script gracefully upon some signals
trap 'panel_clean && exit 0' USR1 INT QUIT TERM EXIT

## Initial cleanup
panel_clean

###
### SECTION: FIFO
###

## Create the FIFO
test -e ${PANEL_FIFO} && rm ${PANEL_FIFO}
mkfifo ${PANEL_FIFO}

## Initialize variables for monitor-specific output
num_mon=$(bspc query -M | wc -l)
wm_info_mon0=''; wm_info_mon1=''; wm_info_mon2=''; wm_info_mon3=''

## FIFO parsing function 
panel_bar()
{
    while read line < ${PANEL_FIFO}
    do
        case ${line} in

            C*) # Clock output
                date="%{F${white}} ${line#?} %{F-}"
                ;;
            B*) # Battery output (function sets its own colours)
                batinfo="${line#?}"
                ;;
            T*) # Xtitle output
                title="%{F${white}}${line#?}%{F-}"
                ;;
            W*) # BSPWM internal state output (RIP 80 columns)
                wm_infos=''
                cur_mon=-1
                IFS=':'
                set -- ${line#?}
                while test ${#} -gt 0
                do
                    item=${1}
                    name=${item#?}
                    case ${item} in
                        M*) # Active monitor
                            cur_mon=$((cur_mon + 1))
                            wm_infos=''
                            if test ${num_mon} -gt 1
                            then
                                wm_infos="${wm_infos} %{F${black}}%{B${blue}} ${name} %{B-}%{F-}  "
                            fi
                            ;;
                        m*) # Inactive monitor
                            cur_mon=$((cur_mon + 1))
                            wm_infos=''
                            if test ${num_mon} -gt 1
                            then
                                wm_infos="${wm_infos} %{F${black}}%{B${yellow}} ${name} %{B-}%{F-}  "
                            fi
                            ;;
                        O*) # Focused occupied desktop
                            wm_infos="${wm_infos}%{F${blue}}%{U${magenta}}%{+u} ${name} %{U-}%{-u}%{F-}"
                            ;;
                        F*) # Focused free desktop
                            wm_infos="${wm_infos}%{U${magenta}}%{+u} ${name} %{U-}%{-u}"
                            ;;
                        U*) # Focused urgent desktop
                            wm_infos="${wm_infos}%{F${red}}%{U${magenta}}%{+u} * %{U-}%{-u}%{F-}"
                            ;;
                        o*) # Occupied desktop
                            wm_infos="${wm_infos}%{F${blue}} ${name} %{F-}"
                            ;;
                        f*) # Free desktop
                            wm_infos="${wm_infos} ${name} "
                            ;;
                        u*) # Urgent desktop
                            wm_infos="${wm_infos}%{F${red}} * %{F-}"
                            ;;
                        #L*) # Layout
                            #    wm_infos="${wm_infos}   %{F${black}}%{B${green}} ${name} %{B-}%{F-}"
                            #    ;;
                    esac
                    shift
                    case ${cur_mon} in
                        0) wm_info_mon0="${wm_infos}";;
                        1) wm_info_mon1="${wm_infos}";;
                        2) wm_info_mon2="${wm_infos}";;
                        3) wm_info_mon3="${wm_infos}";;
                    esac
                done
                ;;
        esac

        if test ${num_mon} -eq 1
        then
            fmt="%{l}${wm_info_mon0} %{c} ${title} %{r} ${batinfo}${date}"
        elif test ${num_mon} -eq 2
        then
            fmt="%{l}${wm_info_mon0} %{c} ${title} %{S+}%{l} ${wm_info_mon1} %{r} ${batinfo}${date}"
        else
            fmt="YOU NEED TO WRITE A FORMAT FOR 3+ MONITORS"
        fi

        printf "%s\n" "${fmt}"
    done
}

##
## SECTION: Lemonbar
##

## Ready BSPWM for the bar
bspc config top_padding ${PANEL_HEIGHT}

## Run the panel itself
xdotool search --sync --name 'bar' set_window --name ${PANEL_MAPPED_NAME} &
panel_bar | lemonbar -g x${PANEL_HEIGHT} -f "${PANEL_FONT}" -F "${foreground}" -B "${background}" &

###
### SECTION: Gather information
###

## Window manager info
bspc control --subscribe > ${PANEL_FIFO} &
xtitle -s -f 'T%.120s' > ${PANEL_FIFO} &

## Clock
clock()
{
    while sleep 1
    do
        date +'C%F %T'
    done;
}
clock > ${PANEL_FIFO} &

## Battery Monitor
if test ! ${BATTERY_LOC} = NONE # If some battery location is specified
then
    bat_percent()
    {
        while true
        do
            if test -e ${BAT}
            then
                BPERCENT=$(echo "(${BATTERY_NOW} * 100) / ${BATTERY_FULL}" | bc)
                if test ${BATTERY_STATUS} = Charging
                then
                    BSTATUS='+'
                elif test ${BATTERY_STATUS} = Discharging
                then
                    BSTATUS='-'
                else
                    BSTATUS=''
                fi

                if test ${BPERCENT} -gt 66
                then
                    BCOLOUR="%{B${green}}"
                elif test ${BPERCENT} -gt 33
                then
                    BCOLOUR="%{B${yellow}}"
                else
                    BCOLOUR="%{B${red}}"
                fi
            else
                BCOLOUR="%{B${yellow}}"
                BSTATUS='A/C'
                BPERCENT=''
            fi
            echo "B%{F${black}}${BCOLOUR} ${BSTATUS}${BPERCENT} %{B-}%{F-}    "
            sleep 10
        done;
    }
    bat_percent > ${PANEL_FIFO} &
fi

###
### SECTION: Wait
###

wait

### EOF
