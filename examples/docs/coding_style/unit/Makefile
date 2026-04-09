SRC:=unit.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -S +unit .

clean:
	rm -f *.cm* *.o a.out
