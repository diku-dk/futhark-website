MIN=-10
MAX=10
SEQSIZES=10000 100000 500000 1000000
PARSIZES=10000 100000 500000 1000000 5000000 10000000 20000000 50000000
# The -I is to use a non-default version of Thrust, for example
# the one from https://github.com/thrust/thrust
#
# This seems to perform better than whatever is distributed with CUDA.
NVCFLAGS=-O3 -I $(HOME)/thrust

.PHONY: clean mss-runtimes
.SECONDARY:

all: graphs/mss.svg

tools/randomarray: tools/randomarray.c
	gcc -o $@ $< -O3

data/%_integers: tools/randomarray
	mkdir -p data && tools/randomarray $(MIN) $(MAX) $* > $@

data/%_scalar:
	mkdir -p data && echo $* > $@

clean:
	rm -rf data
	rm -rf runtimes
	rm -rf compiled
	rm -rf graphs
	rm -f tools/randomarray
	rm -f error.log

compiled/%-futhark-opencl: programs/%.fut
	mkdir -p compiled
	futhark-opencl $< -o $@

compiled/%-futhark-sequential: programs/%.fut
	mkdir -p compiled
	futhark-c $< -o $@

compiled/%-thrust: programs/%.cu
	mkdir -p compiled
	nvcc $< -o $@ $(NVCFLAGS)

runtimes/%-futhark-opencl.runtimes: $(PARSIZES:%=data/%_integers) compiled/%-futhark-opencl
	mkdir -p runtimes
	tools/benchmark-futhark.sh compiled/$*-futhark-opencl $(PARSIZES) 2>>error.log > $@

runtimes/%-futhark-sequential.runtimes: $(SEQSIZES:%=data/%_integers) compiled/%-futhark-sequential
	mkdir -p runtimes
	tools/benchmark-futhark.sh compiled/$*-futhark-sequential $(SEQSIZES) 2>>error.log > $@

runtimes/%-thrust.runtimes: $(PARSIZES:%=data/%_integers) compiled/%-thrust
	mkdir -p runtimes
	tools/benchmark-thrust.sh compiled/$*-thrust $(PARSIZES) 2>>error.log > $@

%-runtimes: runtimes/mss-futhark-opencl.runtimes runtimes/mss-futhark-sequential.runtimes runtimes/mss-thrust.runtimes

graphs/mss.svg: runtimes/mss-futhark-opencl.runtimes runtimes/mss-futhark-sequential.runtimes runtimes/mss-thrust.runtimes
	mkdir -p graphs
	(cat style.gnuplot; \
         echo 'set output "graphs/mss.svg"'; \
         echo 'plot "runtimes/mss-futhark-opencl.runtimes" with linespoints ls 1 title "Futhark",' \
	      '"runtimes/mss-thrust.runtimes" with linespoints ls 2 title "Thrust",' \
	      '"runtimes/mss-futhark-sequential.runtimes" with linespoints ls 3 title "Sequential"' \
	) | gnuplot
