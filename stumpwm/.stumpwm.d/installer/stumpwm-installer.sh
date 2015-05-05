#!/bin/bash

GITURL="https://github.com/stumpwm/stumpwm.git"
QUICKLISPURL="https://beta.quicklisp.org/quicklisp.lisp"
STUMPWMDIR="~/.stumpwm.d/"
LISPFILE="installer-lisp.lisp"
STARTDIR="$PWD"

cd $PWD
cp ${PWD}/${LISPFILE} $HOME

cd $HOME
curl -O $QUICKLISPURL
sbcl --load ${HOME}/${LISPFILE}
rm ${HOME}/quicklisp.lisp ${HOME}/${LISPFILE}

[[ ! -d "$STUMPWMDIR" ]] && mkdir $STUMPWMDIR
git clone ${GITURL} ${STUMPWMDIR}/source/

cd ${STUMPWMDIR}/source/
./autogen.sh
./configure
make
sudo make install
make install-modules
make clean

cd $STARTDIR

# EOF

