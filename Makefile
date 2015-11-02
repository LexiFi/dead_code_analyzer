.PHONY: all opt doc man clean examples check

all:
	make -C src all

opt:
	make -C src opt

doc:
	make -C doc all

man:
	make -C man all

clean:
	make -C src clean
	make -C doc clean
	make -C man clean
	make -C examples clean
	make -C check clean

check: all examples
	make -C check


examples: all
	make -C examples all
