SRC:=with_intf.mli with_intf.ml without_intf.ml use.ml

all: build

build:
	ocamlopt -bin-annot -for-pack To_incl -c ${SRC}
	ocamlopt -pack -o to_incl.cmx $(filter %.cmx, ${SRC:%.ml=%.cmx})
	ocamlopt -a -o to_incl.cmxa to_incl.cmx

clean:
	rm -f *.cm* *.o *.a a.out
