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

# Emacs
alias 'emacsc'='emacs --no-window-system'

SYSTEM=`uname -o`
# echo $SYSTEM
if [[ $SYSTEM =~ "Cygwin" ]]
then
    #viewers
    alias 'ME-View-Viewer-jar'='java -jar c:/Program\ Files/ME-View/bin/ME_VIEW_VIEWER.jar'
    alias 'ME-View-Viewer'='/cygdrive/c/Program\ Files/ME-View/ME_VIEW_VIEWER.exe'
    alias 'WPA-View-Viewer-jar'='java -jar c:/Program\ Files/WPA-View/bin/WPA_VIEW_VIEWER.jar'
    alias 'WPA-View-Viewer'='/cygdrive/c/Program\ Files/WPA-View/WPA_VIEW_VIEWER.exe'
    alias 'PA-View-Viewer-jar'='java -Xmx1024m -jar c:/Program\ Files/PA-View/bin/PA_VIEW_VIEWER.jar'
    alias 'PA-View-Viewer'='/cygdrive/c/Program\ Files/PA-View/PA_VIEW_VIEWER.exe'
    #inline
    alias 'ME-View-jar'='java -jar c:/Program\ Files/ME/bin/ME_VIEW.jar'
    alias 'ME-View'='/cygdrive/c/Program\ Files/ME/ME_VIEW.exe'
    alias 'WPA-View-jar'='java -jar c:/Program\ Files/WPA/bin/WPA_VIEW.jar'
    alias 'WPA-View'='/cygdrive/c/Program\ Files/WPA/WPA_VIEW.exe'
    alias 'PA-View-jar'='java -Xmx1024m -jar c:/Program\ Files/PA/bin/PA_VIEW.jar'
    alias 'PA-View'='/cygdrive/c/Program\ Files/PA/PA_VIEW.exe'

    alias 'findbugs'='java -Dfile.encoding=UTF-8 -jar d:/PA-WPA-View/UTILITIES/findbugs-1.3.9/lib/findbugs.jar'
    alias 'netbeans'='/cygdrive/c/Program\ Files/NetBeans\ 7.1.2/bin/netbeans.exe'
    alias 'ollydbg'='/cygdrive/c/odbg110/OLLYDBG.EXE'
    alias 'VC2008'='/cygdrive/c/Program\ Files/Microsoft\ Visual\ Studio\ 9.0/Common7/IDE/devenv.exe'
    #alias 'msbuild'='msbuild.exe /t:Rebuild /p:Configuration=Release'
    alias 'terminal'='cygstart /cygdrive/c/cygwin/Cygwin.bat'
    alias 'svndiff'='svn diff . --diff-cmd diff -x "-w" | cat -s | grep -3 -e ">" -e "<"'
    alias scilab='"/cygdrive/c/Program Files/scilab-5.2.1/bin/WScilex.exe"'
    alias gem='gem.bat'
    alias rails='rails.bat'
    alias rake='rake.bat'
    alias svn='"/cygdrive/c/Program Files/CollabNet/Subversion Client/svn.exe"'

    #git settings (Windows)
    git config --global core.pager "nkf -s | more"
else
    #git settings (Linux)
    git config --global core.pager "nkf -u | less"
fi

#git settings
alias g="git"
git config --global alias.ci commit
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.rs reset

export REP=https://192.168.20.36/svn
export REP_WPA=${REP}/PA_WPA-View_100
export REP_ME=${REP}/ME-View_210
export REP_LIBS=${REP}/COMMON_LIBS

export SVN_EDITOR=emacsclient
export EDITOR=emacsclient
export VISUAL=emacsclient

export PATH=~/.emacs.d/groovy-1.8.6/bin:$PATH
export PATH=~/.emacs.d/groovyserv/win32/bin:$PATH

#CD path: added software development folder dev
export CDPATH=.:~:~/dev
#Toggle last 2 current directories
# cd -
#Aliases for parent directoriesalias ..="cd .." 
alias ...="cd ../.." 
alias ....="cd ../../.." 
alias .....="cd ../../../.." 
alias ......="cd ../../../../.."
#mkdir+cd command
function mkdircd () { mkdir -p "$@" && eval cd "\"\$$#\""; }
#delsvnunknown
function delsvnunknown () { ~/settings/scripts/delete_unregistered_svn_files.sh $1; }
#change log message
function changesvnlog () { 
    #$1 revision number
    #$2 URL
    #$3 new commit message
    #echo "svn propset -r$1 --revprop svn:log \"$3\" $2"
    svn propset -r$1 --revprop svn:log "$3" $2 
}
#highlight patterns in output (like grep but
#keeping showing the rest of the output
function highlight () {
    pattern=$1; shift; file=$1
    grep -E --color=always "$pattern|$" $file
}
#function to get the list of files in a given state (SVN)
function get_svn_special_state_files() {
    expr=$(echo -n '/^'"$2"'/{print $2}')
    svn stat $1 | awk "$expr" | tr "\\" "/" 2> /dev/null
}
#custom prompt with time
#left param: 0:normal 1:bright/bold 2:dark 4:underlines
#right param: 32:green 33:brown 34:red etc...
export PS1="\e[1;33m[\t]\e[m \e[4;33m\u@\h\e[m \e[1;32m\w\e[m> "

#cd auto spell correction
shopt -s cdspell

#export LANG=ja_JP.UTF-8
#export LC_MESSAGES=ja_JP.UTF-8
#export OUTPUT_CHARSET=utf8
#export TZ=JST-9
#export JLESSCHARSET=japanese-utf8

alias rw='java -classpath dev/Miscellaneous-codes lpsy.other.diet.ReportWeight'
