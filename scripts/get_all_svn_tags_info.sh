#!/bin/bash

base_dir="//$1/Repositories"
svn_base="https://$1/svn"
for dir in `ls $base_dir`
do
    if [ -d $base_dir/$dir/hooks ]
    then
        svn ls $svn_base/$dir/tags > /dev/null 2>&1
        if [ $? == 0 ]
        then
            echo "
###########################
  Repository: ${dir}
###########################
"
            tags=$(svn ls $svn_base/$dir/tags)
            echo "tags: $tags

"
            if [ ! "$tags" == "" ]
            then
                for tag in $tags
                do
                    echo "  >>> $tag
"
                    svn info $svn_base/$dir/tags/$tag
                done
            fi
        fi
    fi
done
