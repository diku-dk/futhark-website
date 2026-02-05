set xlabel "n"
set ylabel "runtime in microseconds"
set grid
set key left top
set logscale x
set xrange [1000:*]
set format x "%.0e"

set terminal pngcairo size 800,600
set output "images/2026-02-04-or.svg"

plot '-' using 1:2 with lines lw 2 title 'or\_reduce', \
     '-' using 1:2 with lines lw 2 title 'or\_scatter'

# Data for Line A
1000 31
10000 31
100000 33
1000000 48
10000000 56
100000000 174
e

# Data for Line B
1000 38
10000 48
100000 50
1000000 62
10000000 164
100000000 737
e
