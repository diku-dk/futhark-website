FUTHARK_BACKEND ?= multicore

all: $(patsubst %.fut, %.md, $(wildcard *.fut))

%.md: %.fut
	rm -rf $*-img
	futhark literate --backend=$(FUTHARK_BACKEND) --stop-on-error $<

.PHONY: clean

clean:
	rm -f $(patsubst %.fut, %.md, $(wildcard *.fut))
