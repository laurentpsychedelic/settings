#!/bin/bash
set -e

function get_all_commits() {
    since=$1
    until=$2
    authors=()
    mapfile -t authors < <(git log --remotes --branches='*' --pretty=format:'%an' $pj | sort | uniq)
    if [ ! ${#authors[@]} -eq 0 ]; then
        #for author in "${authors[@]}"; do
        #    printf "$author,"
        #done
        #printf "\n"
        for author in "${authors[@]}"; do
            n=`git log --remotes --branches='*' --oneline --since=$since --until=$until --author="""$author""" | wc -l`;
            printf "$n,"
        done
        printf "\n"
    fi
}
function get_all_authors() {
    authors=()
    mapfile -t authors < <(git log --remotes --branches='*' --pretty=format:'%an' $pj | sort | uniq)
    if [ ! ${#authors[@]} -eq 0 ]; then
        for author in "${authors[@]}"; do
            echo $author
        done
    fi
}
function print_all_authors() {
    printf ",,,Authors,"
    mapfile -t authors < <(get_all_authors)
    if [ ! ${#authors[@]} -eq 0 ]; then
        for author in "${authors[@]}"; do
            printf "$author,"
        done
    fi
    printf "\n"
}
function main_all_commits_2010_2019_by_quarters() {
    print_all_authors
    for y in {2010..2019}; do
        ds=(4 7 10 1)
        dys=(0 0 0 1)
        for (( i=0; i<=3; i++ )); do
            m="${ds[i]}"
            dy="${dys[i]}"
            since="$((y+dy))/$m/1"
            until="$((y+dy))/$((m+2))/31"
            printf "$y-$((i+1))Q,$since,$until,"
            get_all_commits $since $until
        done
    done
}
function main_all_commits_2018_2019_monthly() {
    print_all_authors
    for y in {2018..2019}; do
       ds=($y/4 $y/5 $y/6 $y/7 $y/8 $y/9 $y/10 $y/11 $y/12 $((y+1))/1 $((y+1))/2 $((y+1))/3);
       for d in "${ds[@]}"; do
           printf "$d,";
           get_all_commits $d/1 $d/31
       done;
    done
}
function main_all_commits_this_month() {
    print_all_authors
    get_all_commits "`date '+%Y/%m'`/1" "`date '+%Y/%m'`/31"
}

function get_projects() {
    echo remotes/origin/develop
    git branch -a | grep remotes/origin/archive
    git branch -a | grep remotes/origin/custom
    git branch -a | grep remotes/origin/project
}
function get_author_stats() {
    pjs=`get_projects`
    since=$1
    until=$2
    authors=()
    for pj in $pjs; do
        if [[ "$pj" =~ "remotes/origin/develop" ]]; then
            base=`git rev-list --max-parents=0 remotes/origin/develop`
        else
            base=`git merge-base $pj remotes/origin/develop`;
        fi
        n=`git log --oneline --since=$since --until=$until $base..$pj | wc -l`;
        if [ $n -ne 0 ]; then
            mapfile -t pj_authors < <(git log --since=$since --until=$until --pretty=format:'%an' $base..$pj | sort | uniq)
            for author in "${pj_authors[@]}"; do
                if [[ ! " ${authors[@]} " =~ " ${author} " ]]; then
                    authors=("${authors[@]}" "$author")
                fi
            done
        fi
    done
    if [ ! ${#authors[@]} -eq 0 ]; then
        printf ","
        for author in "${authors[@]}"; do
            printf "$author,"
        done
        for pj in $pjs; do
            if [[ "$pj" =~ "remotes/origin/develop" ]]; then
                base=`git rev-list --max-parents=0 remotes/origin/develop`
            else
                base=`git merge-base $pj remotes/origin/develop`;
            fi
            n=`git log --oneline --since=$since --until=$until $base..$pj | wc -l`;
            if [ $n -ne 0 ]; then
                printf "\n"
                printf "$pj,"
                for author in "${authors[@]}"; do
                    n_author=`git log --oneline --since=$since --until=$until --author="""$author""" $base..$pj | wc -l`
                    printf "$n_author,"
                done
            fi
        done
        printf "\n"
    fi
}
function main() {
    for y in {2018..2019}; do
        ds=($y/4 $y/5 $y/6 $y/7 $y/8 $y/9 $y/10 $y/11 $y/12 $((y+1))/1 $((y+1))/2 $((y+1))/3);
        for d in "${ds[@]}"; do
            printf "$d\n";
            get_author_stats $d/1 $d/31
        done;
    done
}
