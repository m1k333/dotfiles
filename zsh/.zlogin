#!/bin/zsh

# Add a blank line between linux console login text and first prompt
test "${TERM}" = 'linux' && echo ''

# Start the X server
if test -z "${DISPLAY}" -a ${XDG_VTNR} -eq 1 
then
    startx &> /dev/null
else
    return 0
fi

## EOF
