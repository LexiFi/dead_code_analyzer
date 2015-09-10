COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=foo.mli foo.ml bar.ml baz.mli baz.ml qux.ml ifelse.ml match.ml matchopt.ml\
	 	opt-in-opt.ml useless-opt.ml false/alias-opt.ml false/ref-opt.ml\
	 	unused-fn.ml fooFn.mli fooFn.ml bar-fn.ml

all:
	$(OCAMLC) -c $(SRC)
	make -C false
	../src/dead_code.byt .

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C false clean
