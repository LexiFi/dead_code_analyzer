SRC:=inheritance_lib.mli inheritance_lib.ml inheritance_bin.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -M all .

clean:
	rm -f *.cm* *.o a.out
