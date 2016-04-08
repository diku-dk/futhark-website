MIN=-10
MAX=10
SIZES=10000 100000 500000 1000000

.PHONY: clean
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
	rm -f graphs
	rm -f tools/randomarray
	rm -f error.log

compiled/%-futhark-opencl: programs/%.fut
	mkdir -p compiled
	futhark-opencl $< -o $@

compiled/%-futhark-sequential: programs/%.fut
	mkdir -p compiled
	futhark-c $< -o $@

runtimes/%-futhark-opencl.runtimes: $(SIZES:%=data/%_integers) compiled/%-futhark-opencl
	mkdir -p runtimes
	tools/benchmark-futhark.sh compiled/$*-futhark-opencl $(SIZES) 2>>error.log > $@

runtimes/%-futhark-sequential.runtimes: $(SIZES:%=data/%_integers) compiled/%-futhark-sequential
	mkdir -p runtimes
	tools/benchmark-futhark.sh compiled/$*-futhark-sequential $(SIZES) 2>>error.log > $@

graphs/mss.svg: runtimes/mss-futhark-opencl.runtimes runtimes/mss-futhark-sequential.runtimes
	(cat style.gnuplot; \
         echo 'set output "mss.svg"'; \
         echo 'plot "runtimes/mss-futhark-opencl.runtimes" with linespoints ls 1 title "Futhark",' \
               '"runtimes/mss-futhark-sequential.runtimes" with linespoints ls 3 title "Sequential"') | gnuplot
