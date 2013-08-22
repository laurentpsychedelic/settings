#!/bin/bash

if [ $# -lt 1 ]
then
    file="`cat -`"
fi

if [ $# -lt 0 ]
then
    echo "Get min and max of values in text numerical data"
    echo "  Arguments:"
    echo "    \$1 File (or stdin)"
else
    echo "$file" | awk '
function max(x){i=0;for(val in x){if(i<=x[val]){i=x[val];}}return i;}
function min(x){i=max(x);for(val in x){if(i>x[val]){i=x[val];}}return i;}
/^#/{next}
{a[$6]=$6;next}
END{minimum=min(a);maximum=max(a);print "Maximum = "maximum " and Minimum = "minimum}' $1
fi
