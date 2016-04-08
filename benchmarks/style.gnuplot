set terminal svg size 300,200 dynamic enhanced fname 'arial'  fsize 10 mousing name "MSS_runtime" butt solid
set xlabel "Input elements"
set ylabel "Milliseconds"
set logscale x 10
set key outside
set key above
set grid
set style line 1 lc rgb '#5f021f' lt 1 lw 2 pt 0 ps 1.5   # Futhark
set style line 2 lc rgb '#00aa1f' lt 1 lw 2 pt 0 ps 1.5   # Thrust
set style line 3 lc rgb '#bbbbbb' lt 1 lw 2 pt 0 ps 1.5   # Sequential
