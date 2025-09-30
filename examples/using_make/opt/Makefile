COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC= sig_struct.ml

all:
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
