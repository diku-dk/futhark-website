#!/bin/sh
#
# Utility script for benchmarking Futhark programs.

set -e

prog=$1
shift
sizes=$@

runs=100

average() {
    awk '{sum += strtonum($0)} END{print sum/NR}'
}

echo "# $prog with sizes: $sizes"
for size in $sizes; do
    input=data/${size}_integers
    runtime=$( ($prog < "$input" > /dev/null -r "$runs" -t /dev/fd/8) 8>&1 | average)
    echo "$size $runtime"
done
