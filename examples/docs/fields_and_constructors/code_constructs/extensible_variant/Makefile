SRC:=extensible_variant_lib.mli extensible_variant_lib.ml extensible_variant_bin.ml

all: build analyze

build:
	ocamlopt -w +38 -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -T all .

clean:
	rm -f *.cm* *.o a.out
