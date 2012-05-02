#!/bin/bash
for working_copy in `find . -name working_copy`
do
    pushd $working_copy
    ant clean
    cd ..
    ./update_log.sh
    popd
done