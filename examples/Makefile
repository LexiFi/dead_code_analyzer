.PHONY: clean build

all: build
	dune build ..
	../_build/install/default/bin/dead_code_analyzer $(DEADFLAGS) .

build:
	make -C using_make

clean:
	make -C using_make clean
