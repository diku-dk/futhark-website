#!/bin/sh
#
# Hacky script to extract runtimes from an Accelerate benchmark.

set -e
set -x

prog=$1
shift
sizes=$@

accprog=accelerate-$prog
tmpfile=$(mktemp)

echo "# $prog with sizes: $sizes"
for size in $sizes; do
    $accprog --benchmark --width $size --height $size --csv=$tmpfile > /dev/null
    runtime=$(tail -n 1 $tmpfile | cut -d',' -f2 | awk '{print $1*1000000}')
    echo "$size $runtime"
done

rm -f $tmpfile
