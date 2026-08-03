SRC:=sigincl_lib.mli sigincl_lib.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer .

clean:
	rm -f *.cm* *.o a.out
