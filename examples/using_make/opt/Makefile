COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC= sig_struct.ml
CMTI=$(filter %.cmi,$(SRC:.ml=.cmi)) $(filter %.cmt,$(SRC:.ml=.cmt))

all: $(CMTI)

$(CMTI): $(SRC)
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
