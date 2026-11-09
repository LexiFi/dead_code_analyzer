.PHONY: clean build

COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
CORRECTED_PWD=$(shell if command -v cygpath 2>&1 > /dev/null ; then cygpath -w "$$(pwd)"; else pwd; fi)
SRC=$(CORRECTED_PWD)/absolute.ml
CMTI=$(filter %.cmi,$(SRC:.ml=.cmi)) $(filter %.cmt,$(SRC:.ml=.cmt))

all: build

build: $(CMTI)

$(CMTI): $(SRC)
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj

