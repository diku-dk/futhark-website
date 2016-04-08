#!/bin/sh
#
# Utility script for benchmarking Thrust programs.

set -e

prog=$1
shift
sizes=$@

runs=100

echo "# $prog with sizes: $sizes"
for size in $sizes; do
    runtime=$($prog "$runs" "$size" | grep Runtime | sed -r 's/Runtime: *([0-9]+)us/\1/')
    echo "$size $runtime"
done
