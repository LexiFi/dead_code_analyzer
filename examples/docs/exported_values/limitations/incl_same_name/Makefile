SRC:=oo.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -E all .

clean:
	rm -f *.cm* *.o a.out
