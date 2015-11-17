include $(shell ocamlc -where)/Makefile.config

SRCS=deadCode.mli deadFlag.ml deadCommon.ml deadType.mli \
     deadLexiFi.ml deadType.ml deadArg.ml deadObj.ml deadMod.ml deadCode.ml
LIBS=ocamlcommon.cma

COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

all: deadCode.byt$(EXE)

opt: deadCode.opt$(EXE)

deadCode.byt$(EXE): $(SRCS)
	$(OCAMLC) -o $@ $(LIBS) $(SRCS)

deadCode.opt$(EXE): $(SRCS)
	$(OCAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $(SRCS)

debug: $(SRCS)
	$(OCAMLC) -g -o deadCode.byt$(EXE) $(LIBS) $(SRCS)

prof: $(SRCS)
	ocamlcp -P a $(COMPFLAGS) -o deadCode.byt$(EXE) $(LIBS) $(SRCS)

prof-opt: $(SRCS)
	ocamloptp -P a -p $(COMPFLAGS) -o deadCode.opt$(EXE) $(LIBS:.cma=.cmxa) $(SRCS)

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj deadCode.byt$(EXE) deadCode.opt$(EXE)
