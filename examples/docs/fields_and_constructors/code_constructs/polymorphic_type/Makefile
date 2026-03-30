SRC:=polymorphic_type_lib.mli polymorphic_type_lib.ml polymorphic_type_bin.ml

all: build analyze

build:
	ocamlopt -w +37+69 -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -T all .

clean:
	rm -f *.cm* *.o a.out
