.PHONY: all opt clean examples check

all:
	cd src && make all

opt:
	cd src && make opt

clean:
	cd src && make clean
	cd examples && make clean
	make -C check clean

check:
	make -C src
	make -C examples build
	make -C check


examples: all
	cd examples && make all
