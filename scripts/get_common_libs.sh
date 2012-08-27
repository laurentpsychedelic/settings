#!/bin/bash

if [ -e .svnurl ]; then
    svnurl=$(cat .svnurl)
else
    echo Enter svnurl:
    read svnurl
fi
echo $svnurl > .svnurl

SVN_URL=$svnurl/COMMON_LIBS

for repository in $(svn ls $SVN_URL)
do
    repository=$(echo $repository | sed 's/\///g')
    echo "Loading ${repository}..."
    if [ -e "${repository}.git" ]; then
        echo "Exits! -> delete..."
        rm -rf ${repository}.git 
    fi

    git svn clone ${SVN_URL}/${repository} ${repository}.git --stdlayout #-T ${repository}/trunk -t ${repository}/tags -b ${repository}/branches
    pushd ${repository}.git
    git svn show-ignore >> .git/info/exclude
    popd
done

