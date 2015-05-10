## ~/.zlogin ###########################################################

## Automatically start an X server from zsh login? #####################
STARTX='n'          # 'y' / anything else for no
XCHOICE="stumpwm"   # Args for ~/.xinitrc script

## Start an X server ###################################################
if [[ "$STARTX" == 'y' && -z $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    # Run the startx ~/.xinitrc init script
    startx ~/.xinitrc ${XCHOICE}; fi

## Start a linux console ###############################################
if [[ "$TERM" == 'linux' ]]; then
    # New line for prettiness
    echo ""; fi 

## EOF #################################################################
