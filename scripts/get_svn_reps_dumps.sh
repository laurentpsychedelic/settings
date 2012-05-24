#!/bin/bash

# Text color variables
txtund=$(tput sgr 0 1)          # Underline
txtbld=$(tput bold)             # Bold
bldred=${txtbld}$(tput setaf 1) #  red
bldgrn=${txtbld}$(tput setaf 2) #  green
bldyel=${txtbld}$(tput setaf 3) #  yellow
bldblu=${txtbld}$(tput setaf 4) #  blue
bldpur=${txtbld}$(tput setaf 5) #  purple
bldlbl=${txtbld}$(tput setaf 6) #  light blue
bldwht=${txtbld}$(tput setaf 7) #  white
txtrst=$(tput sgr0)             # Reset
info=${bldwht}*${txtrst}        # Feedback
pass=${bldblu}*${txtrst}
warn=${bldred}*${txtrst}
ques=${bldblu}?${txtrst}

echo $bldpur"+++++++++++++++++++++++"$txtrst
echo $bldpur"+ REPOSITORIES BACKUP +"$txtrst
echo $bldpur"+++++++++++++++++++++++"$txtrst

REMOTE_REPOSITORY_BASEMENT=~/powervault/repositories
LOCAL_REPOSITORY_BASEMENT=~/dev/local_svn_repositories
repositories=$(ls -A $REMOTE_REPOSITORY_BASEMENT)

for repository in ${repositories[@]}
do
    echo ____________________________________________________________________________
    echo -n $txtund"Acquiring dump for ::$bldblu${repository}$txtrst$txtund::"$txtrst
    echo

    echo -n "Execute command: "
    dump_file_name="${repository}_`date +%Y%m%d`.dmp"
    command="svnadmin dump -q ${REMOTE_REPOSITORY_BASEMENT}/${repository} > ${LOCAL_REPOSITORY_BASEMENT}/${dump_file_name}"
    echo ${command}
    svnadmin dump -q ${REMOTE_REPOSITORY_BASEMENT}/${repository} > ${LOCAL_REPOSITORY_BASEMENT}/${dump_file_name}
    #continue
    if [ $? != 0 ]
    then
        echo $bldred"Unable to obtain dump file for repository ::${repository}::"$txtrst
        echo "Abort..."
        if [ -e ${LOCAL_REPOSITORY_BASEMENT}/${dump_file_name} ]
        then
            rm -v ${LOCAL_REPOSITORY_BASEMENT}/${dump_file_name}
        fi
        continue
    else
        echo $bldgrn"OK!"$txtrst
    fi

    echo
    
    echo "Test if local repository ${LOCAL_REPOSITORY_BASEMENT}/${repository} exists..."

    if [ -e ${LOCAL_REPOSITORY_BASEMENT}/${repository} -a -d ${LOCAL_REPOSITORY_BASEMENT}/${repository} ]
    then
        echo $bldred"Found!"$txtrst
        echo "Reinitialize a local repository..."
        rm -rf ${LOCAL_REPOSITORY_BASEMENT}/${repository}
        mkdir ${LOCAL_REPOSITORY_BASEMENT}/${repository}
        command="svnadmin create ${LOCAL_REPOSITORY_BASEMENT}/${repository}"
        echo $command
        $command
        if [ $? != 0 ]
        then
            echo $bldred"Unable to reinitialize repository!"$txtrst
            echo "Abort..."
            continue
        else
            echo $bldgrn"OK!"$txtrst
        fi
    else
        echo $bldyel"Not found!"${END_COLOR}${txtrst}
        echo "Reinitialize a local repository..."
        rm -rf ${LOCAL_REPOSITORY_BASEMENT}/${repository}
        mkdir ${LOCAL_REPOSITORY_BASEMENT}/${repository}
        command="svnadmin create ${LOCAL_REPOSITORY_BASEMENT}/${repository}"
        echo $command
        $command
        if [ $? != 0 ]
        then
            echo $bldred"Unable to initialize repository!"$txtrst
            echo "Abort..."
            continue
        else
            echo $bldgrn"OK!"$txtrst
        fi
        echo
    fi

    echo 
    echo "Load dump file $txtbld${dump_file_name}$txtrst in local repository $txtbld${repository}$txtrst"
    #grep --binary-files=text -v '^* Dumped revision' ${LOCAL_REPOSITORY_BASEMENT}/{dump_file_name} > ${LOCAL_REPOSITORY_BASEMENT}/${dump_file_name}
    command="svnadmin load -q ${LOCAL_REPOSITORY_BASEMENT}/${repository} < ${LOCAL_REPOSITORY_BASEMENT}/${dump_file_name}"
    echo $command
    svnadmin load -q ${LOCAL_REPOSITORY_BASEMENT}/${repository} < ${LOCAL_REPOSITORY_BASEMENT}/${dump_file_name}
    $command
    if [ $? != 0 ]
    then
        echo $bldred"Unable to load dump file !"$txtrst
        continue
    else
        echo $bldgrn"OK!"$txtrst
    fi
done