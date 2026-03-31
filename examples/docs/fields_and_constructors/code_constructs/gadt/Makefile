SRC:=gadt_lib.mli gadt_lib.ml gadt_bin.ml

all: build analyze

build:
	ocamlopt -w +37 -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -T all .

clean:
	rm -f *.cm* *.o a.out
