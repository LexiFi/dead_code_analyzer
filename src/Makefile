include $(shell ocamlc -where)/Makefile.config

SRCS=dead_code.ml
LIBS=ocamlcommon.cma str.cma

COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

all: dead_code.byt$(EXE)

opt: dead_code.opt $(EXE)

dead_code.byt$(EXE): $(SRCS)
	$(OCAMLC) -o $@ $(LIBS) $(SRCS)

dead_code.opt$(EXE): $(SRCS)
	$(OCAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $(SRCS)

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj dead_code.byt$(EXE) dead_code.opt$(EXE)
