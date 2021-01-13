FUTHARK_BACKEND ?= multicore

all: $(patsubst %.fut, %.md, $(wildcard *.fut))

%.md: %.fut
	futhark script --backend=$(FUTHARK_BACKEND) --stop-on-error $<

.PHONY: clean

clean:
	rm -f $(patsubst %.fut, %.md, $(wildcard *.fut))
