#!/bin/sh

# VERSION INFO
HVERSION='2.0.0'

# Continue? (y/n) function
continuep() {
    TRY_AGAIN=true
    CHOICE=true
    while ${TRY_AGAIN}
    do
        echo '*** Continue installation? (y/n)'
        read line
        if test ${line} = 'y'
        then
            TRY_AGAIN=false
        elif test ${line} = 'n'
        then
            CHOICE=false
            TRY_AGAIN=false
        fi
    done
    ${CHOICE} && return 0 || exit 2
}

# Begin
echo '*** Installing Horton for the current user:\n'

# Download source code
echo '*** Downloading source code:'
cd ${HOME}
curl -kLO https://github.com/theochem/horton/releases/download/${HVERSION}/horton-${HVERSION}.tar.gz
tar -xvzf horton-${HVERSION}.tar.gz
cd horton-${HVERSION}

# Install dependencies from official repos
continuep
echo '*** Installing dependencies:'
sudo pacman -S --needed
        gcc gcc-fortran linux-tools matplotlib cython2 python2 \
        python2-h5py python2-numpy python2-scipy python2-sympy \
        python2-nose python2-sphinx python2-matplotlib

# Disable CPU throttling for ATLAS installation
continuep
echo '*** Unthrottling CPU for ATLAS compilation:'
sudo cpupower frequency-set -g performance

# Install ATLAS
continuep
echo '*** Installing ATLAS:'
curl -kLO https://aur.archlinux.org/cgit/aur.git/snapshot/atlas-lapack.tar.gz
tar xvzf atlas-lapack.tar.gz
cd atlas-lapack
makepkg -s
sudo pacman -U atlas-lapack-*.tar.xz
cd ..

# Make Python 2 default:
continuep
echo '*** Symlinking python2 to python:'
sudo ln -s --force /usr/bin/python2 /usr/bin/python

# Configure and install Horton
continuep
echo '*** Installing Horton:'
echo '[blas]' > ./setup.cfg
echo 'libraries=atlas:cblas' >> ./setup.cfg
echo 'include_dirs=/usr/include/atlas' >> ./setup.cfg
./setup.py install --user

# Configure shell
continuep
echo '*** Configuring shell:'
cd ${HOME}
export PATH="${HOME}/.local/bin:${PATH}"
echo '*** Consider adding ~/.local/bin to $PATH in your shell profile.'

# Finish
continuep
echo '*** Testing:'
nosetests2 -v horton && echo '*** Success!  Horton is installed.' \
                     || echo '*** Testing failed!  Investigate!'

# vim:set nowrap ts=2 sw=2 et:
