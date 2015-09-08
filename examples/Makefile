COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=foo.mli foo.ml bar.ml baz.mli baz.ml qux.ml ifelse.ml match.ml matchopt.ml opt-in-opt.ml useless-opt.ml alias-opt.ml

all:
	$(OCAMLC) -c $(SRC)
	../src/dead_code.byt .

clean:
	rm -f *~ *.cm* *.o *.obj
