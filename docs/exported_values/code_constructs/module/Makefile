SRC:=module_lib.mli module_lib.ml module_bin.ml

all: build analyze

build:
	ocamlopt -w +32 -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -E all .

clean:
	rm -f *.cm* *.o a.out
