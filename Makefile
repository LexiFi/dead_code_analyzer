.PHONY: clean examples check dca

dca:
	dune build dead_code_analyzer.install

check:
	make -C check

examples:
	make -C examples

clean:
	dune clean
	make -C examples clean
	make -C check clean
