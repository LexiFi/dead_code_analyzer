SRC:=with_intf.mli with_intf.ml without_intf.mli without_intf.ml use.ml

all: build

build:
	ocamlopt -I ../to_incl -bin-annot to_incl.cmxa ${SRC}

clean:
	rm -f *.cm* *.o a.out
