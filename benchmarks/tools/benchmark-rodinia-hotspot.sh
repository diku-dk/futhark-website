#!/bin/sh

set -e
set -x

sizes=$@

if ! [ -d "$RODINIA_PATH" ]; then
    echo "RODINIA_PATH does not point at a directory." 2>&2
    exit 1
fi

make -C "$RODINIA_PATH/opencl/hotspot" >&2

average() {
    awk '{sum += strtonum($0)} END{print sum/NR}'
}

one_run() {
    size=$1
    temp_file="../../data/hotspot/temp_$size"
    power_file="../..//data/hotspot/power_$size"
    (cd "$RODINIA_PATH/opencl/hotspot";
     "./hotspot" $size 2 360 $temp_file $power_file /dev/null 2>&1) \
        | awk '/Kernel time/{print ($3)*1000000}'
}
echo "$RUNS runs" >&2
for size in $sizes; do
    runtime=$(for x in $(seq "$RUNS"); do one_run $size; done | average)
    echo "$size $runtime"
done
