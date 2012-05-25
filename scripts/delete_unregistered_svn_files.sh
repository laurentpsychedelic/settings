#!/bin/bash
svn stat $1 | awk '/^?/{print $2}' | tr "\\" "/" | xargs rm -rvf
