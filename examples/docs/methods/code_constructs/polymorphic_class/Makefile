SRC:=polymorphic_class_lib.mli polymorphic_class_lib.ml polymorphic_class_bin.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -M all .

clean:
	rm -f *.cm* *.o a.out
