SRC:=functor_lib.mli functor_lib.ml functor_bin.ml

all: build analyze

build:
	ocamlopt -w +37+69 -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -T all .

clean:
	rm -f *.cm* *.o a.out
