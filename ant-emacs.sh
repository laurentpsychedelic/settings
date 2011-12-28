#!/bin/bash
ANT_ARGS="$ANT_ARGS -emacs" 
ANT_OPTS="$ANT_OPTS -Dbuild.compiler.emacs=true" 
export ANT_ARGS ANT_OPTS
#ant $* 2>&1 | gawk -f ~/.emacs.d/transform.gawk 2>&1 | sed s/\\r//g | sed s/^gawk.*$//
ant $* 2>&1 | sed s/\\r//g