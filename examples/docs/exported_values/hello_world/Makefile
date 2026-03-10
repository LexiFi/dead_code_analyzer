.PHONY: clean build

COMPFLAGS=-w +32 -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
SRC=hello_world_no_intf.ml\
	hello_world.mli hello_world.ml\
	hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
CMTI=$(filter %.cmi,$(SRC:.ml=.cmi)) $(filter %.cmt,$(SRC:.ml=.cmt))

all: build

build: $(CMTI)

$(CMTI): $(SRC)
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj

