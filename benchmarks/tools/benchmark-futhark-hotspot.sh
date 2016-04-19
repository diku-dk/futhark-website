#!/bin/sh

set -e
set -x

prog=$1
shift
sizes=$@

average() {
    awk '{sum += strtonum($0)} END{print sum/NR}'
}

echo "# $prog with sizes: $sizes"
for size in $sizes; do
    input=data/${size}_hotspot
    runtime=$( ($prog < "$input" > /dev/null -r "$RUNS" -t /dev/fd/8) 8>&1 | average)
    echo "$size $runtime"
done
