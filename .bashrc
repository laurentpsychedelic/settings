# base-files version 3.7-1

# To pick up the latest recommended .bashrc content,
# look in /etc/defaults/etc/skel/.bashrc

# Modifying /etc/skel/.bashrc directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bashrc) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benificial to all, please feel free to send
# a patch to the cygwin mailing list.

# User dependent .bashrc file


# Shell Options
# #############

# See man bash for more options...

# Don't wait for job termination notification
# set -o notify

# Don't use ^D to exit
# set -o ignoreeof

# Use case-insensitive filename globbing
# shopt -s nocaseglob

# Make bash append rather than overwrite the history on disk
# shopt -s histappend

# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
# shopt -s cdspell


# Completion options
# ##################

# These completion tuning parameters change the default behavior of bash_completion:

# Define to access remotely checked-out files over passwordless ssh for CVS
# COMP_CVS_REMOTE=1

# Define to avoid stripping description in --option=description of './configure --help'
# COMP_CONFIGURE_HINTS=1

# Define to avoid flattening internal contents of tar files
# COMP_TAR_INTERNAL_PATHS=1

# If this shell is interactive, turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
# case $- in
#   *i*) [[ -f /etc/bash_completion ]] && . /etc/bash_completion ;;
# esac


# History Options
# ###############

# Don't put duplicate lines in the history.
# export HISTCONTROL="ignoredups"

# Ignore some controlling instructions
# export HISTIGNORE="[   ]*:&:bg:fg:exit"

# Whenever displaying the prompt, write the previous line to disk
# export PROMPT_COMMAND="history -a"


# Aliases
# #######

# Some example alias instructions
# If these are enabled they will be used instead of any instructions
# they may mask.  For example, alias rm='rm -i' will mask the rm
# application.  To override the alias instruction use a \ before, ie
# \rm will call the real rm not the alias.

# Interactive operation...
# alias rm='rm -i'
# alias cp='cp -i'
# alias mv='mv -i'

# Default to human readable figures
# alias df='df -h'
# alias du='du -h'

# Misc :)
# alias less='less -r'                          # raw control characters
# alias whence='type -a'                        # where, of a sort
alias grep='grep --color'                     # show differences in colour

# Some shortcuts for different directory listings
alias ls='ls -h --color=always'                 # classify files in colour
alias dir='ls --color=always --format=vertical'
alias vdir='ls --color=always --format=long'
alias ll='ls -l'                              # long list
alias la='ls -a'                              # all but . and ..
alias lal='ls -al'
alias l='ls -CF'                              #

# Alias to cyg-get script
alias apt-cyg='bash ~/settings/bin/apt-cyg'

# Emacs
alias 'emacsc'='emacs --no-window-system'

# MSBuild
alias 'msbuild'='MSBuild.exe'

# Dia with toolbox integrated
alias dia='dia --integrated'

SYSTEM=`uname -o`
# echo $SYSTEM
if [[ $SYSTEM =~ "Cygwin" ]]
then
    export CYGWIN=nodosfilewarning
    #kill process on Windows using Powershell
    function kill_win32() {
        powershell "kill -processname $1"
    }
    if [[ ! -e ~/pf ]]
    then
        ln -s "$PROGRAMFILES" ~/pf
    fi
    alias launch=cygstart
    #viewers
    alias 'ME-View-Viewer-jar'='java -jar "$(cygpath -m ~/pf/ME-View/bin/ME_VIEW_VIEWER.jar)"'
    alias 'ME-View-Viewer'='~/pf/ME-View/ME_VIEW_VIEWER.exe'
    alias 'WPA-View-Viewer-jar'='java -jar "$(cygpath -m ~/pf/WPA-View/bin/WPA_VIEW_VIEWER.jar)"'
    alias 'WPA-View-Viewer'='~/pf/WPA-View/WPA_VIEW_VIEWER.exe'
    alias 'PA-View-Viewer-jar'='java -Xmx1024m -jar "$(cygpath -m ~/pf/PA-View/bin/PA_VIEW_VIEWER.jar)"'
    alias 'PA-View-Viewer'='~/pf/PA-View/PA_VIEW_VIEWER.exe'
    #inline
    alias 'ME-View-jar'='java -jar "$(cygpath -m ~/pf/ME/bin/ME_VIEW.jar)"'
    alias 'ME-View'='~/pf/ME/ME_VIEW.exe'
    alias 'WPA-View-jar'='java -jar "$(cygpath -m ~/pf/WPA/bin/WPA_VIEW.jar)"'
    alias 'WPA-View'='~/pf/WPA/WPA_VIEW.exe'
    alias 'PA-View-jar'='java -Xmx1024m -jar "$(cygpath -m ~/pf/PA/bin/PA_VIEW.jar)"'
    alias 'PA-View'='~/pf/PA/PA_VIEW.exe'

    #alias 'findbugs'='java -Dfile.encoding=UTF-8 -jar d:/PA-WPA-View/UTILITIES/findbugs-1.3.9/lib/findbugs.jar'
    alias netbeans='netbeans.exe'
    #alias 'netbeans'='~/pf/NetBeans\ 7.1.2/bin/netbeans.exe'
    #alias 'ollydbg'='/cygdrive/c/odbg110/OLLYDBG.EXE'
    #alias 'VC2008'='~/pf/Microsoft\ Visual\ Studio\ 9.0/Common7/IDE/devenv.exe'
    #alias 'msbuild'='msbuild.exe /t:Rebuild /p:Configuration=Release'
    alias 'terminal'='/cygdrive/c/cygwin/bin/mintty.exe -i /Cygwin-Terminal.ico - &' #'cygstart /cygdrive/c/cygwin/Cygwin.bat'
    #alias scilab='"/cygdrive/c/Program Files/scilab-5.2.1/bin/WScilex.exe"'
    alias gem='gem.bat'
    alias rails='rails.bat'
    alias rake='rake.bat'
    #alias svn='"/cygdrive/c/Program Files/CollabNet/Subversion Client/svn.exe"'
    #a colorized version of SVN diff (requires colordiff command)
    function svndiff () {
        svn diff "${@}" | colordiff | nkf -s | more
    }

    alias emacs='PATH=/c/cygwin/bin:$PATH emacs' # Fix for fork on Cygwin64 with 32bit Emacs

    alias git=`which git` # Hack for Cygwin ::fork:: problem with long PATH
    #git settings (Windows)
    git config --global core.pager "nkf | less"
    export LESSCHARSET=dos # configure less to use DOS charset
    git config --global i18n.commitencoding "SHIFT_JIS"

    #install chocolatey
    function install_chocolatey () {
        powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('http://chocolatey.org/install.ps1'))" && export PATH=$PATH:$systemdrive\chocolatey\bin
    }
    #add chocolatey to path
    export PATH=/cygdrive/c/Chocolatey/bin:$PATH
    #wrapper functions to chocolatey bat scripts
    function chocolatey() {
        chocolatey.bat "${@}"
    }
    function cinst() {
        cinst.bat "${@}"
    }

    #install git-flow
    function install_git-flow() {
        wget -q -O - --no-check-certificate https://github.com/nvie/gitflow/raw/develop/contrib/gitflow-installer.sh | bash
    }

    #function to zip a folder
    function zip_folder() {
        if [ $# -ne 1 ]
        then
            echo "Arguments:"
            echo "\$1 folder to zip"
        else
            folder=$1
            zipfilename=$(basename $folder).zip
            7z a -tzip "$zipfilename" "$folder"
        fi
    }

    #source keychain shell script to enable registered keys
    source ~/.keychain/*-sh

    #add sysinternals tools to the path (sysinternals packages installed with chocolatey)
    export PATH=/cygdrive/c/sysinternals:$PATH

    # Function to 突然死
    function banner() {
        echo-sd "$@" | nkf
    }

    #XWin Tk setting
    export DISPLAY=:0.0
    startxwin > /dev/null 2>&1 &
else
    alias launch=gnome-open
    #git settings (Linux)
    git config --global core.pager "nkf | less"
    #a colorized version of SVN diff (requires colordiff command)
    function svndiff () {
        svn diff "${@}" | colordiff | less -R
    }
    #function to startup chromium-browser
    function chrome () {
        chromium-browser "${@}" > /dev/null 2>&1 &
    }
    #install git-flow
    function install_git-flow() {
        apt-get install git-flow
    }
    #function to install dropbox client
    function install_dropbox() {
        cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86" | tar xzf -
    }
    #function to start dropbox client manually
    function dropboxclient () {
        ~/.dropbox-dist/dropboxd "${@}" > /dev/null 2>&1 &
    }
    #alias to 突然死
    alias banner=echo-sd
fi

# Function to start keychain tool
function start_keychain() {
    for file in $(ls ~/.ssh | awk '/id_rsa.*/ { if (!/.*pub$/) print $1 }')
    do
        keychain ~/.ssh/$file
    done
    source ~/.keychain/*-sh
}


#function to install GVM package manager
function install_gvm() {
    curl -s get.gvmtool.net | bash
}

#git settings
alias g="git"
alias gf="git flow"
alias glog="git log --oneline --decorate"
alias gpull="git pull --recurse-submodules"
git config --global alias.ci commit
git config --global alias.co checkout
git config --global alias.cd checkout
git config --global alias.br branch
git config --global alias.rs reset
git config --global alias.stat status
git config --global alias.st status
git config --global alias.cf config
git config --global alias.lg log
git config --global alias.rb rebase
git config --global alias.cp cherry-pick
git config --global alias.sb submodule
git config --global alias.sm submodule
git config --global core.editor emacsclient
git config --global color.ui true
git config --global alias.track '!f() { ([ $# -eq 2 ] && ( echo "Setting tracking for branch " $1 " -> " $2;git branch --set-upstream $1 $2; ) || ( git for-each-ref --format="local: %(refname:short) <--sync--> remote: %(upstream:short)" refs/heads && echo --Remotes && git remote -v)); }; f'
git config --global user.name "Laurent FABRE"
git config --global log.date local
git config --global core.whitespace trailing-space,space-before-tab
git config --global core.autocrlf input
git config --global core.quotepath off
git config --global push.default upstream
git config --global merge.tool "kdiff3"
git config --global branch.autosetuprebase always
git config --global branch.master.rebase true
git config --global core.fileMode false

export REP=https://192.168.20.36/svn
export REP_WPA=${REP}/PA_WPA-View_100
export REP_ME=${REP}/ME-View_210
export REP_LIBS=${REP}/COMMON_LIBS

export SVN_EDITOR=emacsclient
export EDITOR=emacsclient
export VISUAL=emacsclient

export PATH=~/settings/bin:$PATH

#CD path: added software development folder dev
export CDPATH=.:~:~/dev
#Toggle last 2 current directories
# cd -
#Aliases for parent directoriesalias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."

background='#494949'
function set_background() {
    if [ $# -ne 1 ]
    then
        echo "Arguments:"
        echo "\$1 color  (#HEX)"
    else
        bg="$1"
        echo -ne '\e]11;'$bg'\a'
    fi
}
function find_up() {
    if [ $# -lt 1 ]
    then
        echo "Arguments:"
        echo "\$1 path"
        echo "\$2.. arguments for the find command"
    else
        path="$1"
        shift 1
        while [[ "$path" != "/" ]];
        do
            find "$path"  -maxdepth 1 -mindepth 1 "$@"
            # Note: if you want to ignore symlinks, use "$(realpath -s $path/..)"
            path=$(readlink -f "$path/..")
        done
    fi
}
#change directory and look for change background script
function cd() {
    command cd "$@"
    bgnd=`find_up . -name .bgnd | head -1`
    #echo "bgnd=$bgnd"
    if [ -z "$bgnd" ]; then set_background ${background}; else set_background `cat "${bgnd}"`; fi
}
#mkdir+cd command
function mkdircd () { mkdir -p "$@" && eval cd "\"\$$#\""; }
#set svn ignores
function svn_add_ignore() {
    if [ $# -ne 2 ]
    then
        echo "Arguments:"
        echo "\$1 path"
        echo "\$2 ignores"
    else
        svn propset svn:ignore "`svn propget svn:ignore $1`
$2" $1
    fi
}
#delsvnunknown
function delsvnunknown () { ~/settings/scripts/delete_unregistered_svn_files.sh $1; }
#change SVN commit log message
function change_svn_commit_log_message () {
    if [ $# -ne 3 ]
    then
        echo "Arguments:"
        echo "\$1 revision number"
        echo "\$2 URL"
        echo "\$3 new commit message"
    else
        #echo "svn propset -r$1 --revprop svn:log \"$3\" $2"
        svn propset -r$1 --revprop svn:log "$3" $2
    fi
}
#change SVN commit author
function change_svn_commit_author () {
    if [ $# -ne 3 ]
    then
        echo "Arguments:"
        echo "\$1 revision number"
        echo "\$2 URL"
        echo "\$3 new commit message"
    else
        #echo "svn propset -r$1 --revprop svn:log \"$3\" $2"
        svn propset -r$1 --revprop svn:author "$3" $2
    fi
}
#highlight patterns in output (like grep but
#keeping showing the rest of the output
function highlight () {
    pattern=$1
    if [ $# -gt 1 ]
    then
        shift; file=${@:1}
        grep -a -E --color=always "$pattern|$" "$file"
    else
        grep -a -E --color=always "$pattern|$"
    fi
}
alias hgrep=highlight

#function to get the list of files in a given state (SVN)
function get_svn_special_state_files() {
    expr=$(echo -n '/^'"$2"'/{print $2}')
    svn stat $1 | awk "$expr" | tr "\\" "/" 2> /dev/null
}
#function to cleanup .svn folders of a SVN working copy
function clean_svn_folders() {
    if [ $# -gt 1 ]
    then
        place="$1"
    else
        place=.
    fi
    clean_folders_regexp $place \.svn
}
#function to cleanup .git folders of a Git repository
function clean_git_folders() {
    if [ $# -gt 1 ]
    then
        place="$1"
    else
        place=.
    fi
    clean_folders_regexp $place \.git
}

#clean ~ backup files
function clean_backup_files() {
    if [ $# -gt 1 ]
    then
        place=$1
    else
        place=.
    fi
    clean_files_regexp $place \*~
}
#clean flymake temporary files
function clean_flymake_files() {
    if [ $# -gt 1 ]
    then
        place=$1
    else
        place=.
    fi
    clean_files_regexp $place \*flymake\*
}
#clean class files
function clean_class_files() {
    if [ $# -gt 1 ]
    then
        place=$1
    else
        place=.
    fi
    clean_files_regexp $place \*class
}
#clean junk files
function clean_junk_files() {
    file="."
    if [ $# -gt 1 ]
    then
        file=$1
    fi
    clean_backup_files $file
    clean_flymake_files $file
    clean_class_files $file
}
#clean files matching given regular expression
function clean_files_regexp() {
    find "$1" -name "$2" | xargs rm -vf
}
#clean folders matching given regular expression
function clean_folders_regexp() {
    find "$1" -name "$2" | xargs rm -rvf
}
#generate GTAGS
function generate_gtags() {
    find . -type f -regex '.*\(java$\|h$\|hpp$\|cpp$\|HPP$\|hpp$\)' -print0 | xargs -0 etags
}
#function to get changes from SVN in Git repositories (through git-svn)
function git_svn_update() {
    git svn rebase && git svn fetch --all
}
#function for git diff -w --cached (difference between HEAD and index
function git_diff_w_cached() {
    git diff -w -M --cached $@
}
#function to get all changed files list (Git)
function git_ls_files_m() {
    git ls-files -m $@
}
#function to get all changed files and add them to index (Git)
function git_ls_files_m_add() {
    git ls-files -m $@ | xargs git add
}

#function to cherry pick git commits
function git_cherry_pick() {
    git cherry-pick $@
}
#function to cherry pick git commits (force "--theirs" changes)
function git_cherry_pick_force() {
    git cherry-pick $@ --strategy=recursive -X theirs
}

#function to cherry-pick (git) all related commits (based on a keyword on the FIRST line of the commit message) in specified branch
function cherry_pick_all_related_commits {
    if [ $# -lt 2 ]
    then
        echo "Arguments:"
        echo "\$1 Branch to scan for commit"
        echo "\$1 Keyword to search in commit message (first line)"
    else
        BRANCH=$1
        KEYWORD=$2
        ACTION='git_cherry_pick'
        ARGS=($@)
        OPTIONS=(${ARGS[@]:2})
        for option in ${OPTIONS[@]}
        do
            if [[ $option =~ "--force" ]]
            then
                ACTION='git_cherry_pick_force'
            fi
        done
        do_all_related_commits $BRANCH $KEYWORD $ACTION --reverse
    fi
}

#function to cherry-pick (git) all related commits (based on a keyword on the FIRST line of the commit message) in specified branch
function do_all_related_commits {
    if [ $# -lt 3 ]
    then
        echo "Arguments:"
        echo "\$1 Branch to scan for commit"
        echo "\$2 Keyword to search in commit message (first line)"
        echo "\$3 Action to do on each commit"
    else
        BRANCH=$1
        KEYWORD=$2
        ACTION=$3
        ARGS=($@)
        OPTIONS=(${ARGS[@]:3})
        commits=$(eval show_all_related_commits $BRANCH $KEYWORD $OPTIONS)
        for commit in $commits
        do
            bash -ic "$ACTION $commit"
        done
    fi
}

#function to show all related commits (based on a keyword on the FIRST line of the commit message) in specified branch
function show_all_related_commits {
    if [ $# -lt 2 ]
    then
        echo "Arguments:"
        echo "\$1 Branch to scan for commit"
        echo "\$2 Keyword to search in commit message (first line)"
    else
        BRANCH=$1
        KEYWORD=$2
        FILTER=cat
        if [[ $# -gt 2 ]]
        then
            ARGS=($@)
            OPTIONS=(${ARGS[@]:2})
            LONG=""
            for option in ${OPTIONS[@]}
            do
                if [[ $option =~ "--reverse" ]]
                then
                    FILTER=tac
                fi
                if [[ $option =~ "--long" ]]
                then
                    LONG=yes
                fi
            done
        fi

        if [ -z "$LONG" ]
        then
            git log --oneline --decorate $BRANCH | grep $KEYWORD | awk '//{print $1}' | $FILTER
        else
            git log --oneline --decorate $BRANCH | grep $KEYWORD | awk '//{print}' | $FILTER
        fi
    fi
}

#function to update all git repositories in a given directory
function git_update_all() {
    ~/settings/scripts/update_all.sh $@
}

#function to get a list of all the different author in the current git branch
function git_get_authors() {
    git shortlog -sne $@
    # git log | grep Author | sort | uniq
}

#function to get stats by author in a Git repository
function git_get_stats() {
    git log --shortstat --pretty="%cE" | sed 's/\(.*\)@.*/\1/' | grep -v "^$" | awk 'BEGIN { line=""; } !/^ / { if (line=="" || !match(line, $0)) {line = $0 "," line }} /^ / { print line " # " $0; line=""}' | sort | sed -E 's/# //;s/ files? changed,//;s/([0-9]+) ([0-9]+ deletion)/\1 0 insertions\(+\), \2/;s/\(\+\)$/\(\+\), 0 deletions\(-\)/;s/insertions?\(\+\), //;s/ deletions?\(-\)//' | awk 'BEGIN {name=""; files=0; insertions=0; deletions=0;} {if ($1 != name && name != "") { print name ": " files " files changed, " insertions " insertions(+), " deletions " deletions(-), " insertions-deletions " net"; files=0; insertions=0; deletions=0; name=$1; } name=$1; files+=$2; insertions+=$3; deletions+=$4} END {print name ": " files " files changed, " insertions " insertions(+), " deletions " deletions(-), " insertions-deletions " net";}'
}

#function to commit Emacs abbrevs changes automatically
function git_commit_Emacs_abbrevs() {
    git add ~/settings/.emacs.d/abbrev_defs
    git ci -m "Updated Emacs abbrevs."
}

#function to get and register svn ignores in repository local ignores (in .git dir)
function git_svn_register_ignores() {
    git svn show-ignore > .git/info/exclude
}

#function to filter git branch to change commits' author name and email
function git_filter_branch_author() {
    if [ $# -ne 3 ]
    then
        echo "Arguments:"
        echo "\$1 old name"
        echo "\$2 new name"
        echo "\$3 new email"
    else
        old_name=$1
        new_name=$2
        new_email=$3
        git filter-branch -f --commit-filter 'if [ "$GIT_AUTHOR_NAME" = "'"$old_name"'" ];
            then
                GIT_AUTHOR_NAME="'"$new_name"'";
                GIT_AUTHOR_EMAIL="'"$new_email"'";
                git commit-tree "$@";
            else
                git commit-tree "$@";
            fi' -- --all
    fi
}

#function to show a view of current repo (alternative to normal git-log)\
function glog() {
    git log --all --pretty='format:%d %Cgreen%h%Creset %an - %s' --graph
}

#list email setings in current repository and all submodules
function git_list_user_settings() {
    echo "*** <global> ***" && git config --global -l | grep -e 'user.name' -e 'user.email'
    echo "*** <top-level> ***"; git config --local -l | grep -e 'user.name' -e 'user.email'
    for sb in `g sb | awk '//{print $2}'`; do echo "*** $sb ***"; git config --local -l | grep -e 'user.name' -e 'user.email'; done
}

#list email setings in current repository and all submodules
function git_list_email_settings() {
    echo "*** <global> ***" && git config --global -l | grep 'user.email'
    echo "*** <top-level> ***"; git config --local -l | grep 'user.email'
    for sb in `g sb | awk '//{print $2}'`; do echo "*** $sb ***"; git config --local -l | grep 'user.email'; done
}

#list email setings in current repository and all submodules
function git_set_email_settings() {
    if [ $# -ne 1 ]
    then
        echo "Arguments:"
        echo "\$1 Email address to set"
    else
        echo "*** <top-level> ***"; git config --local user.email $1
        for sb in `g sb | awk '//{print $2}'`; do echo "*** $sb ***"; git config --local user.email $1; done
    fi
}

# function to check make result (Java project)
function make_check() {
    make rebuild $@ && echo '!!!!!!!! <OK> !!!!!!!!' && ls -al dist/*.jar && if [[ -d plugins ]]; then ls -al plugins/*.jar; fi
}

#function to show multiple files side by side
function cat2() {
    pr -m -t "$@"
}

function less2() {
    cat2 "$@" | less
}

#function to two files differences side by side
function diff2() {
    sdiff "$@" | sed -r 's/[<>|]//;s/(\t){3}//' | less
}

#function to preview markdown files located in current folder
function preview_markdown() {
    #require Python grip package: sudo pip install grip
    grip
}

#function to generate the ISO image based on the contents of a given folder
function generate_ISO_image() {
    if [ $# -ne 2 ]
    then
        echo "Arguments:"
        echo "\$1 ISO file name"
        echo "\$2 folder to be ISOified"
    else
        genisoimage -o "$1" -J -joliet-long -r -l "$2"
    fi
}

#function to generate an ISO image of all directories at the current location
function generate_ISO_images() {
    for dir in `find . -mindepth 1 -maxdepth 1 -type d`
    do
        generate_ISO_image "${dir}.iso" "${dir}"
    done
}

#function to backup a folder as tar.gz file to a given location
function backup_folder() {
    if [ $# -ne 2 ]
    then
        echo "Arguments:"
        echo "\$1 location of the folder to be backed up"
        echo "\$2 location of the storage in which the backup file will be stored"
    else
        name=$(basename "$1")
        # echo "${2}/${name}_$(date '+%Y%m%d').tar.gz" "$1"
        tar czvf "${2}/${name}_$(date '+%Y%m%d').tar.gz" "$1"
    fi
}

# function to call Dropbox CLI
function dropbox() {
    python ~/settings/scripts/dropbox.py $@
}

# function to store and reorder pictures in given location, with the folder structure %YEAR/%MONTH/%DAY
function store_and_reorder_pictures() {
    if [ $# -ne 2 ]
    then
        echo "Arguments:"
        echo "\$1 location of the folder to find pictures"
        echo "\$2 location of the storage in which the files will be stored and reordered"
    else
        store_and_reorder_items $1 $2 '-iname \*.jpg -o -iname \*.png'
    fi
}

# function to store and reorder movies in given location, with the folder structure %YEAR/%MONTH/%DAY
function store_and_reorder_movies() {
    if [ $# -ne 2 ]
    then
        echo "Arguments:"
        echo "\$1 location of the folder to find movies"
        echo "\$2 location of the storage in which the files will be stored and reordered"
    else
        store_and_reorder_items $1 $2 '-iname \*.mov'
    fi
}

# function to store and reorder pictures and movies in given location, with the folder structure %YEAR/%MONTH/%DAY
function store_and_reorder_pictures_and_movies() {
    if [ $# -ne 2 ]
    then
        echo "Arguments:"
        echo "\$1 location of the folder to find pictures and movies"
        echo "\$2 location of the storage in which the files will be stored and reordered"
    else
        store_and_reorder_items $1 $2 '-iname \*.jpg -o -iname \*.png -o -iname \*.mov'
    fi
}

function store_and_reorder_items() {
    if [ ! `which exiftool` ]
    then
        echo 'ExifTool not found! Please install...'
        return 2
    fi

    if [ $# -ne 3 ]
    then
        echo "Arguments:"
        echo "\$1 location of the folder to find pictures"
        echo "\$2 location of the storage in which the files will be stored and reordered"
        echo "\$3 regexp identifying items"
        return 1
    fi

    in_location=$1
    out_location=$2
    regexp=$3

    find_command=`echo find "${in_location}" -type f "${regexp}"`
    # echo find_command:{${find_command}}

    SAVEIFS=$IFS
    IFS=`echo -en "\n\b"` ; files=`eval ${find_command}` ; IFS=$SAVEIFS
    for file in $files
    do
        echo "File: \"${file}\""
        date=`exiftool -s3 -CreateDate "${file}" | awk '//{print $1}' | tr ':' '/'`
        [[ "${date}" == "" ]] && date=unknown
        if [ -z ${date} ]
        then
            date=unknown
        fi
        out_folder=${out_location}/${date}
        if [ ! -d ${out_folder} ]
        then
            # echo mkdir -p "${out_folder}"
            mkdir -p "${out_folder}"
        fi
        # echo rsync -azvr "${file}" "${out_folder}/"
        rsync -azvr "${file}" "${out_folder}/"
        # echo
    done
}

#custom prompt with time
#left param: 0:normal 1:bright/bold 2:dark 4:underlines
#right param: 32:green 33:brown 34:red etc...
export PS1="\[\e[1;33m\][\t]\[\e[m\] \[\e[4;33m\]\u@\h\[\e[m\] \[\e[1;32m\]\w\[\e[m\]> "

#cd auto spell correction
shopt -s cdspell

#export LANG=ja_JP.UTF-8
#export LC_MESSAGES=ja_JP.UTF-8
#export OUTPUT_CHARSET=utf8
#export TZ=JST-9
#export JLESSCHARSET=japanese-utf8

alias rw='java -classpath dev/Miscellaneous-codes lpsy.other.diet.ReportWeight'

# Added by autojump install.sh
# source /etc/profile.d/autojump.bash

# Scripts folder in path
PATH=$PATH:~/settings/scripts

#Load GVM
if [[ -s "$(readlink -f ~/settings/scripts/.gvm_load)" ]]
then
    source "$(readlink -f ~/settings/scripts/.gvm_load)"
fi
