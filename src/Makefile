include $(shell ocamlc -where)/Makefile.config

SRCS=deadCode.mli deadCode.ml
LIBS=ocamlcommon.cma

COMPFLAGS=-w +A-4-9-40-42-50 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

all: deadCode.byt$(EXE)

opt: deadCode.opt $(EXE)

deadCode.byt$(EXE): $(SRCS)
	$(OCAMLC) -o $@ $(LIBS) $(SRCS)

deadCode.opt$(EXE): $(SRCS)
	$(OCAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $(SRCS)

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj deadCode.byt$(EXE) deadCode.opt$(EXE)
