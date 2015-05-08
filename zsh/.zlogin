## ~/.zlogin

## Start X at login?
XCHOICE='no' # 'stumpwm', 'emacs', etc., or 'no'

## Set up our linux console
[[ "$TERM" == 'linux' ]] && echo '' # Pretty printing!

## Stop if X is unwanted or if we're in SSH
[[ "$XCHOICE" -eq 'no' || -n "$SSH_CLIENT" || -n "$SSH_TTY" ]] && return

## Start X if a display server is not already running
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx ~/.xinitrc $XCHOICE

## EOF
