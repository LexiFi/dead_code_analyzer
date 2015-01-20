.PHONY: all opt clean examples

all:
	cd src && make all

opt:
	cd src && make opt

clean:
	cd src && make clean
	cd examples && make clean


examples: all
	cd examples && make all
