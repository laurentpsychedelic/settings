#!/bin/bash
if [ -e .remote_repository_url ]; then
    REMOTE_REPOSITORY_URL=$(cat .remote_repository_url)
else
    echo Enter remote repositories URL:
    read REMOTE_REPOSITORY_URL
fi
echo $REMOTE_REPOSITORY_URL > .remote_repository_url

LOCAL_REPOSITORY_URL=file:///home/laurent/dev/local_svn_repositories
repositories=( ME-View_210 PA_WPA-View_100 COMMON_LIBS )
relocate_local() {
    #$1 repository name
    echo "switch --relocate ${REMOTE_REPOSITORY_URL}/$1 ${LOCAL_REPOSITORY_URL}/$1 $2"
    svn switch --relocate ${REMOTE_REPOSITORY_URL}/$1 ${LOCAL_REPOSITORY_URL}/$1 $2
}
relocate_remote() {
    #$1 repository name
    echo "svn switch --relocate ${LOCAL_REPOSITORY_URL}/$1 ${REMOTE_REPOSITORY_URL}/$1 $2"
    svn switch --relocate ${LOCAL_REPOSITORY_URL}/$1 ${REMOTE_REPOSITORY_URL}/$1 $2
}

if [[ $1 =~ --help ]]
then
    echo "Relocate all repositories (named \"working copy\") to:"
    echo
    echo "        --local    local repository (in ${LOCAL_REPOSITORY_URL})"
    echo "        --remote   remote repository (in ${REMOTE_REPOSITORY_URL})"
    exit
elif [[ $1 = --local || $1 =~ --remote ]]
then
    echo
    #nothing
else
    echo $bldred"Unknown option $txtund${1}$txtrst$bldred!"$txtrst
    echo "Try with --help option for help."$txtrst
    exit
fi


for working_copy in `find . -name working_copy`
do
    res=$(svn info $working_copy)
    for repository in ${repositories[@]}
    do
        if [[ $res =~ ${repository} ]]
        then
            #echo "${working_copy}=>${repository}"
            if [[ $1 =~ --local  ]]
            then
                relocate_local ${repository} ${working_copy}
            elif [[ $1 =~ --remote ]]
            then
                relocate_remote ${repository} ${working_copy}
            fi
        fi
    done
done