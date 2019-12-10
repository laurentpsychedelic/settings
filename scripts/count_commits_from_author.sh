#!/bin/bash

if [ $# -lt 3 ]
then
    echo "Wrong number of arguments!"
    echo "    \$1: <tree-ish>"
    echo "    \$2: --since:<date>"
    echo "    \$3: --until:<date>"
    echo "    \$4: <author>"
    exit
fi

target=$1
since=$2
until=$3
author=$4
if [ -z $4 ]
then
    git log $target --since=$since --until=$until --pretty=format:'%an' | sort | wc -l
else
    git log $target --since=$since --until=$until --pretty=format:'%an' | sort | grep -i $author | wc -l
fi
