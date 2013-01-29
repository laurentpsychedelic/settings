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
    #HARD LINK TO .SCREENRC
    FILE_LNK=$(cygpath -w $HOME_DIR/.screenrc)
    FILE_TAR=$(cygpath -w $SETTINGS_DIR/.screenrc)
    junction -s "$FILE_LNK" "$FILE_TAR"

    #SET SHELL ENV TO BASH.EXE
    profile=$(cat /etc/profile)
    if [[ "$profile" =~ "SHELL='c:/cygwin/bin/bash.exe'" ]]
    then
        echo "SHELL already set..."
    else
        echo "Set SHELL to bash.exe :: export SHELL='c:/cygwin/bin/bash.exe'" #>> /etc/profile
        echo "" >> /etc/profile
        echo "export SHELL='c:/cygwin/bin/bash.exe'" >> /etc/profile
    fi
    #PREADD COLLABNET SVN Client TO THE PATH
    if [[ "$profile" =~ "CollabNet/Subversion Client" ]]
    then
        echo "SVN client path already set..."
    else
        echo "Set SVN client path to CollabNet client :: export PATH='/cygdrive/c/Program Files/CollabNet/Subversion Client':$PATH"
        echo "" >> /etc/profile
        echo "export PATH='/cygdrive/c/Program Files/CollabNet/Subversion Client':\$PATH" >> /etc/profile
    fi

    #CREATE SYMBOLIC LINK TO %PROGRAMFILES%
    junction -s "$(cygpath -w ~/pf)" "$PROGRAMFILES"

    #CREATE SYMBOLIC LINKS TO CYGWIN HOME FOLDER
    FILE_LNK="C:/home"
    FILE_TAR="C:/cygwin/home"
    junction -s "$FILE_LNK" "$FILE_TAR"

    #CREATE SYMBOLIC LINK TO DROPBOX FOLDER
    if [ -e "$USERPROFILE/Dropbox" ]
    then
        FILE_LNK=$(cygpath -m ~/Dropbox)
        FILE_TAR=$(cygpath -m $USERPROFILE/Dropbox)
        junction -s "$FILE_LNK" "$FILE_TAR"
    fi

    #INSTALL AUTOJUMP
    cd ~/settings/bin/autojump_v19
    ./install.sh --bash
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

    #SOFT LINK TO UPDATE
    FILE_LNK=$HOME_DIR/.update
    FILE_TAR=$SETTINGS_DIR/scripts/update
    if [ -e "$FILE_LNK" ]; then
	mv -vf $FILE_LNK ${FILE_LNK}_old
    fi
    ln -s $FILE_TAR $FILE_LNK

    #INSTALL AUTOJUMP
    cd ~/settings/bin/autojump_v19
    ./install.sh
fi

#ADD COMPLEMENTARY HISTORY SEARCHING CAPABILITIES FOR BASH IN .INPUTRC 
inputrc=$(cat ~/.inputrc)
if [[ "$inputrc" =~ "\e[A" ]]
then
    echo "History searching capabilities already set in .inputrc..."
else
    echo "Add history searching capabilities for bash in .inputrc"
    echo "
" >> ~/.inputrc
    echo '"\e[A": history-search-backward
"\e[B": history-search-forward
set show-all-if-ambiguous on
set completion-ignore-case on
' >> ~/.inputrc
fi


source ~/.bashrc
source /etc/profile
