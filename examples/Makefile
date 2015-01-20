COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

all:
	$(OCAMLC) -c foo.mli foo.ml bar.ml
	../src/dead_code.byt .

clean:
	rm -f *~ *.cm* *.o *.obj
