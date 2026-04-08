SRC:=bind.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -S +bind .

clean:
	rm -f *.cm* *.o a.out
