SRC:=polymorphic_variant_lib.mli polymorphic_variant_lib.ml polymorphic_variant_bin.ml

all: build analyze

build:
	ocamlopt -w +37 -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -T all .

clean:
	rm -f *.cm* *.o a.out
