SRC:=modtype_with_intf.mli modtype_with_intf.ml modtype_without_intf.ml modtype_bin.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --all -E threshold:4 -M threshold:4 -T threshold:4 .

clean:
	rm -f *.cm* *.o a.out
