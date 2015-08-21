#!/bin/sh
# vim: set nowrap:

##
## Source panelrc
##

. ${HOME}/.bspwm/panelrc

##
## Clean-up
##

panel_clean()
{
    # Clean up older panels (keeping this instance)
    local PROC_COUNT=$(pgrep -cx panel.sh)
    while test ${PROC_COUNT} -gt 1
    do
        pkill -ox panel.sh
        PROC_COUNT=$(pgrep -cx panel.sh)
    done

    # Remove padding bspc padding
    bspc config top_padding 0

    # Remove old FIFO
    rm -f /tmp/bspwm/panel_fifo

    # Done
    return 0
}

trap 'panel_clean && exit 0' INT QUIT TERM EXIT
panel_clean

##
## Set up FIFO and parse its output
##

test -d /tmp/bspwm || mkdir -p /tmp/bspwm
mkfifo /tmp/bspwm/panel_fifo

num_mon=$(bspc query -M | wc -l)
wm_info_mon0=''
wm_info_mon1=''
wm_info_mon2=''
wm_info_mon3=''

panel_parse() {
  # *** OUTER WHILE ***
  while read -r line < /tmp/bspwm/panel_fifo
  do

  # *** OUTER CASE ***
  case ${line} in

  C*) # Clock output
  date="${line#?}"
  ;;

  N*) # Notification output
  notice="${line#?}"
  ;;

  #B*) # Battery output (function sets its own colours)
  #batinfo="${line#?}"
  #;;

  W*) # BSPWM internal state output
  wm_infos=''
  cur_mon=-1
  IFS=':'
  set -- ${line#?}

  # *** INNER WHILE ***
  while test ${#} -gt 0
  do

  item=${1}
  name=${item#?}

  # *** INNER CASE ***
  case ${item} in

  #M*) # Active monitor
  #cur_mon=$((cur_mon + 1))
  #wm_infos=''
  #wm_infos="${wm_infos} %{F${black}}%{B${blue}} ${name} %{B-}%{F-}  "
  #;;

  #m*) # Inactive monitor
  #cur_mon=$((cur_mon + 1))
  #wm_infos=''
  #wm_infos="${wm_infos} %{F${black}}%{B${yellow}} ${name} %{B-}%{F-}  "
  #;;

  O*) # Focused occupied desktop
  wm_infos="${wm_infos}%{F${blue}} ${name} %{F-}"
  ;;

  F*) # Focused free desktop
  wm_infos="${wm_infos}%{F${blue}} ${name} %{F-}"
  ;;

  U*) # Focused urgent desktop
  wm_infos="${wm_infos}%{F${red}} ${name} %{F-}"
  ;;

  o*) # Occupied desktop
  wm_infos="${wm_infos}%{F${yellow}} ${name} %{F-}"
  ;;

  f*) # Free desktop
  wm_infos="${wm_infos}%{F${white}} ${name} %{F-}"
  ;;

  u*) # Urgent desktop
  wm_infos="${wm_infos}%{F${red}} ${name} %{F-}"
  ;;

  #L*) # Layout
  #wm_infos="${wm_infos}   %{F${black}}%{B${green}} ${name} %{B-}%{F-}"
  #;;

  # *** DONE INNER CASE ***
  esac

  shift

  #case ${cur_mon} in
  #    0) wm_info_mon0="${wm_infos}";;
  #    1) wm_info_mon1="${wm_infos}";;
  #    2) wm_info_mon2="${wm_infos}";;
  #    3) wm_info_mon3="${wm_infos}";;
  #esac

  # *** DONE INNER WHILE ***
  done
  ;;

  # *** DONE OUTER CASE ***
  esac

  fmt="%{l} ${notice}%{c}${wm_infos}%{r}${batinfo}${date} "
  printf "%s\n" "${fmt}"

  # *** DONE OUTER WHILE ***
  done
}

##
## Pipe the parser into the panel
##

# Start the panel
panel_parse | lemonbar -g x${PANEL_HEIGHT} -f "${PANEL_FONT}" \
              -u ${PANEL_UNDERLINE} -F "${white}" -B "${black}" &

# Pad BSPWM
bspc config top_padding ${PANEL_HEIGHT}

##
## Run some info-gathering programs into the FIFO
##

statnot ${HOME}/.bspwm/statnot.py &
bspc control --subscribe > /tmp/bspwm/panel_fifo &
while sleep 1; do date +'C%F %T' > /tmp/bspwm/panel_fifo; done &
#. ${HOME}/.bspwm/battery.sh && battery_monitor > /tmp/bspwm/panel_fifo &

##
## Wait for a signal
##

wait

## EOF
