# This Makefile is a total mess.  I tried to maintain some sort of
# structure, but eh.  Benchmarking is always a dirty business.

# Path to unmodified Rodinia
RODINIA_PATH=$(HOME)/rodinia_3.1
export RODINIA_PATH

RUNS=100
export RUNS

MIN=-10
MAX=10
sum_SEQSIZES=100000 500000 1000000 2000000 5000000
sum_PARSIZES=100000 500000 1000000 2000000 5000000 10000000 20000000 30000000 100000000
mss_SEQSIZES=100000 200000 500000 800000 1000000
mss_PARSIZES=100000 200000 500000 800000 1000000 2000000 5000000 10000000
mandelbrot_SEQSIZES=10 20 50 100
mandelbrot_PARSIZES=10 20 50 100 200 300 400 500 600 700 800 900 1000
hotspot_SEQSIZES=64 128
hotspot_PARSIZES=64 512 1024
# The -I is to use a non-default version of Thrust, for example
# the one from https://github.com/thrust/thrust
#
# This seems to perform better than whatever is distributed with CUDA.
NVCFLAGS=-O3 -I $(HOME)/thrust

.PHONY: clean
.SECONDARY:

all: graphs/sum.svg graphs/mss.svg graphs/mandelbrot.svg

tools/randomarray: tools/randomarray.c
	gcc -o $@ $< -O3

data/%_integers: tools/randomarray
	mkdir -p data && tools/randomarray $(MIN) $(MAX) $* > $@

data/%_scalar:
	mkdir -p data && echo $* > $@

# Warning: total hack
data/128_hotspot:
	mkdir -p data
	echo 360 > $@
	cat $(RODINIA_PATH)/data/hotspot/temp_512 | head -n 16384 | tools/futharkise-hotspot-array.py /dev/stdin >> $@
	cat $(RODINIA_PATH)/data/hotspot/power_512 | head -n 16384 | tools/futharkise-hotspot-array.py /dev/stdin >> $@

data/%_hotspot:
	mkdir -p data
	echo 360 > $@
	tools/futharkise-hotspot-array.py $(RODINIA_PATH)/data/hotspot/temp_$* >> $@
	tools/futharkise-hotspot-array.py $(RODINIA_PATH)/data/hotspot/power_$* >> $@

clean:
	rm -rf data
	rm -rf runtimes
	rm -rf compiled
	rm -rf graphs
	rm -f tools/randomarray
	rm -f error.log

compiled/%-futhark-opencl: programs/%.fut
	mkdir -p compiled
	futhark-opencl programs/$*.fut -o $@

compiled/%-futhark-sequential: programs/%.fut
	mkdir -p compiled
	futhark-c programs/$*.fut -o $@

compiled/%-thrust: programs/%.cu
	nvcc programs/$*.cu -o $@ $(NVCFLAGS)

runtimes/mandelbrot-futhark-opencl.runtimes: compiled/mandelbrot-futhark-opencl
	mkdir -p runtimes
	tools/benchmark-futhark.sh scalar compiled/mandelbrot-futhark-opencl $(mandelbrot_PARSIZES) 2>>error.log > $@
runtimes/mandelbrot-futhark-sequential.runtimes: compiled/mandelbrot-futhark-sequential
	mkdir -p runtimes
	tools/benchmark-futhark.sh scalar compiled/mandelbrot-futhark-sequential $(mandelbrot_SEQSIZES) 2>>error.log > $@
runtimes/mandelbrot-accelerate.runtimes:
	mkdir -p runtimes
	tools/benchmark-accelerate.sh mandelbrot $(mandelbrot_PARSIZES) 2>>error.log > $@

runtimes/hotspot-futhark-opencl.runtimes: hotspot-dataset compiled/hotspot-futhark-opencl
	mkdir -p runtimes
	tools/benchmark-futhark-hotspot.sh compiled/hotspot-futhark-opencl $(hotspot_PARSIZES) 2>>error.log > $@

runtimes/hotspot-futhark-sequential.runtimes: hotspot-dataset compiled/hotspot-futhark-sequential
	mkdir -p runtimes
	tools/benchmark-futhark-hotspot.sh compiled/hotspot-futhark-sequential $(hotspot_SEQSIZES) 2>>error.log > $@

runtimes/hotspot-rodinia.runtimes:
	mkdir -p runtimes
	tools/benchmark-rodinia-hotspot.sh $(hotspot_PARSIZES) 2>>error.log > $@

runtimes/%-futhark-opencl.runtimes: %-par-dataset compiled/%-futhark-opencl
	mkdir -p runtimes
	tools/benchmark-futhark.sh compiled/$*-futhark-opencl $($*_PARSIZES) 2>>error.log > $@

runtimes/%-futhark-sequential.runtimes: %-seq-dataset compiled/%-futhark-sequential
	mkdir -p runtimes
	tools/benchmark-futhark.sh compiled/$*-futhark-sequential $($*_SEQSIZES) 2>>error.log > $@

runtimes/%-thrust.runtimes: %-par-dataset compiled/%-thrust
	mkdir -p runtimes
	tools/benchmark-thrust.sh compiled/$*-thrust $($*_PARSIZES) 2>>error.log > $@

mss-runtimes: runtimes/mss-futhark-opencl.runtimes runtimes/mss-futhark-sequential.runtimes runtimes/mss-thrust.runtimes

sum-runtimes: runtimes/sum-futhark-opencl.runtimes runtimes/sum-futhark-sequential.runtimes runtimes/sum-thrust.runtimes

mandelbrot-runtimes: runtimes/mandelbrot-futhark-opencl.runtimes runtimes/mandelbrot-futhark-sequential.runtimes runtimes/mandelbrot-accelerate.runtimes

hotspot-runtimes: runtimes/hotspot-rodinia.runtimes runtimes/hotspot-futhark-opencl.runtimes

graphs/mss.svg: runtimes/mss-futhark-opencl.runtimes runtimes/mss-futhark-sequential.runtimes runtimes/mss-thrust.runtimes
	mkdir -p graphs
	(cat style.gnuplot; \
         echo 'set output "graphs/mss.svg"'; \
         echo 'plot "runtimes/mss-futhark-opencl.runtimes" with linespoints ls 1 title "Futhark",' \
	      '"runtimes/mss-thrust.runtimes" with linespoints ls 2 title "Thrust",' \
	      '"runtimes/mss-futhark-sequential.runtimes" with linespoints ls 3 title "Sequential"' \
	) | gnuplot

graphs/sum.svg: runtimes/sum-futhark-opencl.runtimes runtimes/sum-futhark-sequential.runtimes runtimes/sum-thrust.runtimes
	mkdir -p graphs
	(cat style.gnuplot; \
         echo 'set output "graphs/sum.svg"'; \
         echo 'plot "runtimes/sum-futhark-opencl.runtimes" with linespoints ls 1 title "Futhark",' \
	      '"runtimes/sum-thrust.runtimes" with linespoints ls 2 title "Thrust",' \
	      '"runtimes/sum-futhark-sequential.runtimes" with linespoints ls 3 title "Sequential"' \
	) | gnuplot

graphs/mandelbrot.svg: runtimes/mandelbrot-futhark-opencl.runtimes runtimes/mandelbrot-futhark-sequential.runtimes runtimes/mandelbrot-accelerate.runtimes runtimes/mandelbrot-thrust.runtimes
	mkdir -p graphs
	(cat style.gnuplot; \
         echo 'set output "graphs/mandelbrot.svg"'; \
         echo 'unset logscale x'; \
         echo 'set xlabel "Width and height of visualisation"'; \
         echo 'plot "runtimes/mandelbrot-futhark-opencl.runtimes" with linespoints ls 1 title "Futhark",' \
	      '"runtimes/mandelbrot-thrust.runtimes" with linespoints ls 2 title "Thrust",' \
	      '"runtimes/mandelbrot-accelerate.runtimes" with linespoints ls 4 title "Accelerate",' \
	      '"runtimes/mandelbrot-futhark-sequential.runtimes" with linespoints ls 3 title "Sequential"' \
	) | gnuplot

graphs/hotspot.svg: runtimes/hotspot-futhark-opencl.runtimes runtimes/hotspot-futhark-sequential.runtimes runtimes/hotspot-rodinia.runtimes
	mkdir -p graphs
	(cat style.gnuplot; \
         echo 'set output "graphs/hotspot.svg"'; \
         echo 'unset logscale x'; \
         echo 'set xlabel "Grid edge size"'; \
         echo 'plot "runtimes/hotspot-futhark-opencl.runtimes" with linespoints ls 1 title "Futhark",' \
	      '"runtimes/hotspot-futhark-sequential.runtimes" with linespoints ls 3 title "Sequential",' \
	      '"runtimes/hotspot-rodinia.runtimes" with linespoints ls 5 title "Rodinia"' \
	) | gnuplot


sum-par-dataset: $(sum_PARSIZES:%=data/%_integers)
sum-seq-dataset: $(sum_SEQSIZES:%=data/%_integers)
mss-par-dataset: $(mss_PARSIZES:%=data/%_integers)
mss-seq-dataset: $(mss_SEQSIZES:%=data/%_integers)
mandelbrot-par-dataset:
mandelbrot-seq-dataset:
hotspot-dataset: $(hotspot_SEQSIZES:%=data/%_hotspot) $(hotspot_PARSIZES:%=data/%_hotspot)
