.PHONY: clean run_dca build_dca build_dune build_make build dune_only make_only

CMTI_ROOT_MAKE=using_make
CMTI_ROOT_DUNE=using_dune/_build/default/
CMTI_ROOT?=$(CMTI_ROOT_MAKE) $(CMTI_ROOT_DUNE)

all: build build_dca run_dca

make_only: build_make build_dca
	make run_dca CMTI_ROOT=$(CMTI_ROOT_MAKE)

dune_only: build_dune build_dca
	make run_dca CMTI_ROOT=$(CMTI_ROOT_DUNE)

build: build_make build_dune

build_make:
	make -C using_make

build_dune:
	dune build --root=using_dune @check

build_dca:
	make -C .. dca

run_dca:
	../_build/install/default/bin/dead_code_analyzer $(DEADFLAGS) $(CMTI_ROOT)

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C using_make clean
	dune clean --root=using_dune
