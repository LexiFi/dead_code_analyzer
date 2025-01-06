.PHONY: all opt doc man clean examples check

all:
	make -C src all

opt:
	make -C src opt

lexifi:
	make -C src lexifi

lexifi-opt:
	make -C src lexifi-opt

man:
	make -C man all

clean:
	make -C src clean
	make -C man clean
	make -C examples clean
	make -C check clean
	rm -rf build

check:
	make -C check


examples: all
	make -C examples all
