#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIR=$(dirname $DIR)
mv -v $HOME/.emacs.d $HOME/.emacs.d_old
ln -s -v $DIR $HOME/.emacs.d