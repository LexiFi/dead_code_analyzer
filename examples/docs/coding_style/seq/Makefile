SRC:=seq.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -S +seq .

clean:
	rm -f *.cm* *.o a.out
