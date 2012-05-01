#!/bin/bash

SYSTEM=`uname -o`
# echo $SYSTEM
if [[ $SYSTEM =~ "Cygwin" ]]
then
    echo "Windows!"
    SETTINGS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    SETTINGS_DIR=$(dirname $SETTINGS_DIR)
    SETTINGS_DIR=`cygpath -w $SETTINGS_DIR`
    HOME_DIR=`cygpath -w ~`
    #HARD LINK TO .BASHRC
    FILE_LNK=$(cygpath -w $HOME_DIR/.bashrc)
    FILE_TAR=$(cygpath -w $SETTINGS_DIR/.bashrc)
    junction -s $FILE_LNK $FILE_TAR
    #HARD LINK TO .EMACS.D
    FILE_LNK=$(cygpath -w $HOME_DIR/.emacs.d)
    FILE_TAR=$(cygpath -w $SETTINGS_DIR/.emacs.d)
    junction -s $FILE_LNK $FILE_TAR
else
    echo "Linux!"
    DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    DIR=$(dirname $DIR)

    #SOFT LINK TO 
    mv -v $HOME/.emacs.d $HOME/.emacs.d_old
    ln -s -v $DIR $HOME/.emacs.d
fi

