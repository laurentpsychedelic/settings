#!/bin/bash

if [ -z $1 ]
then
    echo "Wrong number of arguments!"
    echo "You must provide the location in which repositories are to be found..."
    exit
fi

root_dir=$(readlink -f $1)
echo "Update all repositories in < $root_dir >"

# Find all .git repositories in <home>/dev directory
for dir in $(find $root_dir -maxdepth 6 -type d -name .git)
do
    # Full path to the repository
    eval dir=$(readlink -f $dir/..)
    echo ">>> Update repository at: " $dir

    # go to Git repository
    pushd $dir > /dev/null

    # Update
    if [ -e "$dir/.git/svn" ]
    then
        echo "    This is a git-svn aware repository"
        # git-svn update command
        git svn rebase && git svn fetch --all
    else
        echo "    This is NOT a git-svn aware repository"
        # git pull command
        git pull
    fi

    #return back where we were
    popd > /dev/null
done
