COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=mod.mli mod.ml incl.ml func.mli func.ml use_func.ml val.mli inc_val.mli val.ml inc_val.ml
CMTI=$(filter %.cmi,$(SRC:.ml=.cmi)) $(filter %.cmt,$(SRC:.ml=.cmt))

all: $(CMTI)

$(CMTI): $(SRC)
	$(OCAMLC) -c $(SRC)

build:
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
