#!/bin/bash

if [ $# -lt 2 ]
then
    file="`cat -`"
fi

if [ $# -lt 1 ]
then
    echo "Get position (row, col) of value specified by pattern in 2D text numerical data"
    echo "  Arguments:"
    echo "    \$1 Pattern to search"
    echo "    \$2 File (or stdin)"
else
    echo "$file" | awk "
END { if (!f++) print \"pattern not found\" }
/$1/{ for(i=1;i<=NF;i++){
if (\$i ~ \"$1\")
{print \"[line=\"NR,\"column=\"i-1\"]\", f++} }
}" $2
fi
