#!/bin/bash

if [ -e .svnurl ]; then
    svnurl=$(cat .svnurl)
else
    echo Enter svnurl:
    read svnurl
fi
echo $svnurl > .svnurl

if [ -e .local_svnurl ]; then
    local_svnurl=$(cat .local_svnurl)
else
    echo Enter local_svnurl:
    read local_svnurl
fi
echo $local_svnurl > .local_svnurl

REMOTE_URL=$svnurl/COMMON_LIBS
#FILE_URL=file:///home/laurent/dev/local_svn_repositories/COMMON_LIBS
FILE_URL=$local_svnurl/COMMON_LIBS
for repository in $(svn ls $FILE_URL)
do
    repository=$(echo $repository | sed 's/\///g')
    echo "Loading ${repository}..."
    if [ -e "${repository}.git" ]; then
        echo "Exits! -> delete..."
        rm -rf ${repository}.git 
    fi
    mkdir ${repository}.git
    pushd ${repository}.git
    git svn init -T ${repository}/trunk -t ${repository}/tags -b ${repository}/branches --rewrite-root $REMOTE_URL $FILE_URL
    git svn fetch
    git config svn-remote.svn.url $REMOTE_URL/$repository
    git svn show-ignore >> .git/info/exclude
    popd
done

