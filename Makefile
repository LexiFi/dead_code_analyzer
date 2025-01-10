.PHONY: clean examples check

all:
	dune build

check:
	make -C check

examples:
	make -C examples

clean:
	make -C examples clean
	make -C check clean
