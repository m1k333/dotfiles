## ~/.zlogin

## Start X at login?
STARTXATLOGIN='no' # 'x', 'emacs', or anything else for 'no'

## Set up our linux console
if [[ "$TERM" == 'linux' ]] ; then
    [[ -e "/usr/share/kbd/consolefonts/ter-u20n.psf.gz" ]] && setfont ter-u20n
    echo # Pretty printing!
fi

## Start default X server (+ safety "are we in console?" check)
if [[ "$STARTXATLOGIN" == 'x' ]] ; then
    [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

## Same, but run Emacs fullscreen for our X session
elif [[ "$STARTXATLOGIN" == 'emacs' ]] ; then
    [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx ~/.xinitrc emacs

## Done
fi
