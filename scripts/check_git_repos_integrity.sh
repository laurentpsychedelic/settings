#!/bin/bash

if [ -z $1 ]
then
    echo "Wrong number of arguments!"
    echo "You must provide the location in which repositories are to be found..."
    exit
fi

root_dir=$(readlink -f $1)
echo "Check all repositories in < $root_dir >"

# Find all .git repositories in <home>/dev directory
for dir in $(find $root_dir -maxdepth 6 -type d -name hooks -o -name .git)
do
    dir=$(dirname $dir)
    # Full path to the repository
    eval dir=$(readlink -f $dir)
    if [[ "$(basename $dir)" == .git || "$dir" == */.git/* ]]
    then
        # echo "[ $dir already checked, passing... ]"
        continue
    # else
        # echo "[ $dir not already checked, checking... ]"
    fi
    echo ">>> Check repository at: " $dir

    # go to Git repository
    pushd $dir > /dev/null

    # git fsck command
    git fsck && echo 'OK!'

    #return back where we were
    popd > /dev/null
done
