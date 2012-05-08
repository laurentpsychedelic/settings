#!/bin/bash

SYSTEM=`uname -o`
# echo $SYSTEM
if [[ $SYSTEM =~ "Cygwin" ]]
then
    echo "Windows!"

    SETTINGS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    SETTINGS_DIR=$(dirname $SETTINGS_DIR)
    SETTINGS_DIR=`cygpath -w $SETTINGS_DIR`

    PATH=$SETTINGS_DIR/bin:$PATH

    HOME_DIR=`cygpath -w ~`
    #HARD LINK TO .BASHRC
    FILE_LNK=$(cygpath -w $HOME_DIR/.bashrc)
    FILE_TAR=$(cygpath -w $SETTINGS_DIR/.bashrc)
    junction -s "$FILE_LNK" "$FILE_TAR"
    #HARD LINK TO .EMACS.D
    FILE_LNK=$(cygpath -w $HOME_DIR/.emacs.d)
    FILE_TAR=$(cygpath -w $SETTINGS_DIR/.emacs.d)
    junction -s "$FILE_LNK" "$FILE_TAR"
else
    echo "Linux!"
    SETTINGS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    SETTINGS_DIR=$(dirname $SETTINGS_DIR)
    HOME_DIR=$HOME

    #SOFT LINK TO .BASHRC
    FILE_LNK=$HOME_DIR/.bashrc
    FILE_TAR=$SETTINGS_DIR/.bashrc
    if [ -e "$FILE_LNK" ]; then
	mv -vf $FILE_LNK ${FILE_LNK}_old
    fi
    ln -s $FILE_TAR $FILE_LNK

    #SOFT LINK TO .EMACS.D
    FILE_LNK=$HOME_DIR/.emacs.d
    FILE_TAR=$SETTINGS_DIR/.emacs.d
    if [ -e "$FILE_LNK" ]; then
	mv -vf $FILE_LNK ${FILE_LNK}_old
    fi
    ln -s $FILE_TAR $FILE_LNK
fi

source ~/.bashrc