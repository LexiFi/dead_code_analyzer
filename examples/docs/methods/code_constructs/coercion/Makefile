SRC:=coercion_lib.mli coercion_lib.ml coercion_bin.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -M all .

clean:
	rm -f *.cm* *.o a.out
