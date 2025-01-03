.PHONY: all opt doc man clean examples check

all:
	make -C src all

opt:
	make -C src opt

lexifi:
	make -C src lexifi

lexifi-opt:
	make -C src lexifi-opt

doc:
	make -C doc all

man:
	make -C man all

debug:
	make -C src debug

prof:
	make -C src prof

prof-opt:
	make -C src prof-opt

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
