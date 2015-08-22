#!/bin/sh
# vim: set nowrap:

. ${HOME}/.bspwm/panelrc

# Clean up older instances
panel_clean()
{
    local PROC_COUNT=$(pgrep -cx panel.sh)
    while test ${PROC_COUNT} -gt 1
    do
        pkill -ox panel.sh
        PROC_COUNT=$(pgrep -cx panel.sh)
    done
    bspc config top_padding 0
    rm -f /tmp/bspwm/panel_fifo
    return 0
}

# Trap some term signals to exit cleanly
trap 'panel_clean && exit 0' INT QUIT TERM EXIT
panel_clean

# Create FIFO, initialize formatting string
test -d /tmp/bspwm || mkdir -p /tmp/bspwm
mkfifo /tmp/bspwm/panel_fifo

# Function to toggle multiple monitor behaviour
get_monitor_setup()
{
    if ${MULTIPLE_MONITORS}
    then
        MULTIPLE_MONITORS=false
    else
        MULTIPLE_MONITORS=true
        # Force status update
        (sleep 1 && bspc control --get-status > /tmp/bspwm/panel_fifo) &
    fi
}

# Function to get BSPWM layout in parser
get_layout()
{
    MONOCLEP=$(bspc query  -T -d focused | awk 'FNR == 2 {print $5}')
    FLOATINGP=$(bspc query -T -d focused | awk 'FNR == 2 {print $6}')
    test "${FLOATINGP}" = 'f' && layout='F'          || layout='T'
    test "${MONOCLEP}"  = 'M' && layout="${layout}M" || layout="${layout}D"
}

# Parse input
panel_parse()
{
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

  B*) # Battery output (function sets its own colours)
  batinfo="${line#?}"
  ;;

  R*) # Toggle multiple monitor behaviour
  get_monitor_setup
  ;;

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

  M*) # Active monitor
  cur_mon=$((cur_mon + 1))
  mon_infos="%{F${black}}%{B${blue}} ${name} %{B-}%{F-}"
  ;;

  m*) # Inactive monitor
  cur_mon=$((cur_mon + 1))
  mon_infos="%{F${black}}%{B${cyan}} ${name} %{B-}%{F-}"
  ;;

  O*) # Focused occupied desktop
  get_layout
  wm_infos="${wm_infos}%{F${yellow}}%{U${blue}}%{+u} ${name} %{-u}%{F-}"
  ;;

  F*) # Focused free desktop
  get_layout
  wm_infos="${wm_infos}%{U${blue}}%{+u} ${name} %{-u}%{U-}"
  ;;
  [Uu]*) # Urgent desktop
  wm_infos="${wm_infos}%{F${red}} ${name} %{F-}"
  ;;

  o*) # Occupied desktop
  wm_infos="${wm_infos}%{F${yellow}} ${name} %{F-}"
  ;;

  f*) # Free desktop
  wm_infos="${wm_infos} ${name} "
  ;;

  # *** DONE INNER CASE ***
  esac

  shift

  if ${MULTIPLE_MONITORS}
  then
    case ${cur_mon} in
       0)
       wm_info_mon0="${wm_infos}"; mon_info_mon0="${mon_infos}"
       layout_mon0="${layout}"
       ;;
       1)
       wm_info_mon1="${wm_infos}"; mon_info_mon1="${mon_infos}"
       layout_mon1="${layout}"
       ;;
       2)
       wm_info_mon2="${wm_infos}"; mon_info_mon2="${mon_infos}"
       layout_mon2="${layout}"
       ;;
    esac
  fi

  # *** DONE INNER WHILE ***
  done
  ;;

  # *** DONE OUTER CASE ***
  esac
  
  if ${MULTIPLE_MONITORS} then
  then
    num_mon=$(bspc query -M | wc -l)
    case ${num_mon} in
    1)
    fmt="%{l} [${layout_mon0}] ${notice}%{c}${mon_info_mon0}   ${wm_info_mon0}%{r}[${batinfo}] ${date} "
    ;;
    2)
               fmt="%{l} [${layout_mon0}] ${notice}%{c}${mon_info_mon0}   ${wm_info_mon0}%{r}[${batinfo}] ${date} "
    fmt="${fmt}%{S+}%{l} [${layout_mon1}] ${notice}%{c}${mon_info_mon1}   ${wm_info_mon1}%{r}[${batinfo}] ${date} "
    ;;
    3)
               fmt="%{l} [${layout_mon0}] ${notice}%{c}${mon_info_mon0}   ${wm_info_mon0}%{r}[${batinfo}] ${date} "
    fmt="${fmt}%{S+}%{l} [${layout_mon1}] ${notice}%{c}${mon_info_mon1}   ${wm_info_mon1}%{r}[${batinfo}] ${date} "
    fmt="${fmt}%{S+}%{l} [${layout_mon2}] ${notice}%{c}${mon_info_mon2}   ${wm_info_mon2}%{r}[${batinfo}] ${date} "
    ;;
    esac
  else
    fmt="%{l} [${layout}] ${notice}%{c}${wm_infos}%{r}[${batinfo}] ${date} "
  fi
  printf "%s\n" "${fmt}"

  # *** DONE OUTER WHILE ***
  done
}

# Run the panel
bspc config top_padding ${PANEL_HEIGHT}
panel_parse | lemonbar -g x${PANEL_HEIGHT} -f "${PANEL_FONT}" \
    -u ${PANEL_UNDERLINE} -F "${white}" -B "${black}" &

# Run programs into FIFO
statnot ${HOME}/.bspwm/statnot.py &
bspc control --subscribe > /tmp/bspwm/panel_fifo &
while sleep 1
do
    date +'C%F %T' > /tmp/bspwm/panel_fifo
done &
. ${HOME}/.bspwm/battery.sh && battery_monitor > /tmp/bspwm/panel_fifo &

wait

## EOF
