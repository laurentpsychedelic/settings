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
# alias ls='ls -hF --color=tty'                 # classify files in colour
# alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -a'                              # all but . and ..
alias lal='ls -al'
alias l='ls -CF'                              #

alias 'ME-View'='~/ME-View'
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
alias 'netbeans'='/cygdrive/c/Program\ Files/NetBeans\ 7.1.1/bin/netbeans.exe'
alias 'ollydbg'='/cygdrive/c/odbg110/OLLYDBG.EXE'
alias 'VC2008'='/cygdrive/c/Program\ Files/Microsoft\ Visual\ Studio\ 9.0/Common7/IDE/devenv.exe'

# Functions
# #########

# Some example functions
# function settitle() { echo -ne "\e]2;$@\a\e]1;$@\a"; }

alias excel='~/shortcuts/MS_office/excel'
alias powerpoint='~/shortcuts/MS_office/powerpoint'
alias word='~/shortcuts/MS_office/word'
alias acroread='~/shortcuts/acroread'

alias 'dev'='~/dev'

alias 'msbuild'='msbuild.exe /t:Rebuild /p:Configuration=Release'

alias 'terminal'='cygstart /cygdrive/c/cygwin/Cygwin.bat'

# Emacs
alias 'emacsc'='emacs --no-window-system'
# GNU global
alias 'ctags'='/cygdrive/c/Program\ Files/emacs-23.3/bin/ctags.exe'
alias 'etags'='/cygdrive/c/Program\ Files/emacs-23.3/bin/etags.exe'

alias 'svndiff'='svn diff . --diff-cmd diff -x "-w" | cat -s | grep -3 -e ">" -e "<"'
alias scilab='"/cygdrive/c/Program Files/scilab-5.2.1/bin/WScilex.exe"'

alias gem='gem.bat'
alias rails='rails.bat'
alias rake='rake.bat'

alias svn='"/cygdrive/c/Program Files/CollabNet/Subversion Client/svn.exe"'

alias g="git"

export REP=https://192.168.20.36/svn
export REP_WPA=${REP}/PA_WPA-View_100
export REP_ME=${REP}/ME-View_210
export REP_LIBS=${REP}/COMMON_LIBS

export SVN_EDITOR=emacsclient
export EDITOR=emacsclient
export VISUAL=emacsclient

#export LANG=ja_JP.UTF-8
#export LC_MESSAGES=ja_JP.UTF-8
#export OUTPUT_CHARSET=utf8
#export TZ=JST-9
#export JLESSCHARSET=japanese-utf8

alias rw='java -classpath dev/Miscellaneous-codes lpsy.other.diet.ReportWeight'