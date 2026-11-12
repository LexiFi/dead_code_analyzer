.PHONY: clean build

COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
SRC=..//relative//.////relative.ml
CMTI=$(filter %.cmi,$(SRC:.ml=.cmi)) $(filter %.cmt,$(SRC:.ml=.cmt))

all: build

build: $(CMTI)

$(CMTI): $(SRC)
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
