COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)SRC=class.mli class.ml inher.mli inher.ml fooCl.mli fooCl.ml barCl.mli barCl.ml object.ml
SRC=class.mli class.ml inher.mli inher.ml fooCl.mli fooCl.ml barCl.mli barCl.ml\
overr.mli overr.ml object.ml

all:
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
