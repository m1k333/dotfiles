# ~/.zlogin

## Uncomment to start default X server automatically upon login
# [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

## Uncomment to start Emacs X session automatically upon login
# [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx ~/.xinitrc emacs

## Do things in the linux console
if [[ "$TERM" == 'linux' ]] ; then

    ## Load the linux console font I like
    setfont ter-u20n

    ## Make the messages pretty by inserting a blank line!
    echo

fi

# EOF
