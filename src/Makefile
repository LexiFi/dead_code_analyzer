include $(shell ocamlc -where)/Makefile.config

SRCFST=deadCode.mli deadFlag.ml deadCommon.ml deadArg.mli deadType.mli deadObj.mli
SRCSND=deadType.ml deadArg.ml deadMod.ml deadObj.ml deadCode.ml
LIBS=ocamlcommon.cma

COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

all: deadCode.byt$(EXE)

opt: deadCode.opt$(EXE)

deadCode.byt$(EXE): $(SRCFST) $(SRCSND)
	$(OCAMLC) -o $@ $(LIBS) $(SRCFST) $(SRCSND)

deadCode.opt$(EXE): $(SRCFST) $(SRCSND)
	$(OCAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $(SRCFST) $(SRCSND)

lexifi: $(SRCFST) deadLexiFi.ml $(SRCSND)
	$(OCAMLC) -o deadCode.byt$(EXE) $(LIBS) $(SRCFST) deadLexiFi.ml $(SRCSND)

lexifi-opt:$(SRCFST) deadLexiFi.ml $(SRCSND)
	$(OCAMLOPT) -o deadCode.opt$(EXE) $(LIBS:.cma=.cmxa) $(SRCFST) deadLexiFi.ml $(SRCSND)

debug: $(SRCFST) $(SRCSND)
	$(OCAMLC) -g -o deadCode.byt$(EXE) $(LIBS) $(SRCFST) $(SRCSND)

prof: $(SRCFST) $(SRCSND)
	ocamlcp -P a $(COMPFLAGS) -o deadCode.byt$(EXE) $(LIBS) $(SRCFST) $(SRCSND)

prof-opt: $(SRCFST) $(SRCSND)
	ocamloptp -P a -p $(COMPFLAGS) -o deadCode.opt$(EXE) $(LIBS:.cma=.cmxa) $(SRCFST) $(SRCSND)

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj deadCode.byt$(EXE) deadCode.opt$(EXE)
