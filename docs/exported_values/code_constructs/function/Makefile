SRC:=function_lib.mli function_lib.ml function_bin.ml

all: build analyze

build:
	ocamlopt -w +27+32 -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -E all .

clean:
	rm -f *.cm* *.o a.out
