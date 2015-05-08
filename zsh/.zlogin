## ~/.zlogin

## Start X at login?
STARTXATLOGIN='no' # 'x', 'emacs', or anything else for 'no'

## Remote login?  Do nothing!
[[ -n "$SSH_CLIENT" || -n "$SSH_TTY" ]] && return

## Set up our linux console
[[ "$TERM" == 'linux' ]] && echo # Pretty printing!

## Start default X server (+ safety "are we in console?" check)
if [[ "$STARTXATLOGIN" == 'x' ]] ; then
    [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

## Same, but run Emacs fullscreen for our X session
elif [[ "$STARTXATLOGIN" == 'emacs' ]] ; then
    [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx ~/.xinitrc emacs

## Done
fi
