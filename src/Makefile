include $(shell ocamlc -where)/Makefile.config

SRCFST=deadCode.mli deadFlag.ml deadCommon.ml deadArg.mli deadType.mli deadObj.mli
SRCSND=deadType.ml deadArg.ml deadMod.ml deadObj.ml deadCode.ml
LIBS=ocamlcommon.cma

COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs -g
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

all: dead_code_analyzer.byt$(EXE)

opt: dead_code_analyzer.opt$(EXE)

dead_code_analyzer.byt$(EXE): $(SRCFST) $(SRCSND)
	$(OCAMLC) -o $@ $(LIBS) $(SRCFST) $(SRCSND)
	cp -rf . ../build
	make clean

dead_code_analyzer.opt$(EXE): $(SRCFST) $(SRCSND)
	$(OCAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $(SRCFST) $(SRCSND)
	cp -rf . ../build
	make clean

lexifi: $(SRCFST) deadLexiFi.ml $(SRCSND)
	$(OCAMLC) -o dead_code_analyzer.byt$(EXE) $(LIBS) $(SRCFST) deadLexiFi.ml $(SRCSND)
	cp -rf . ../build
	make clean

lexifi-opt:$(SRCFST) deadLexiFi.ml $(SRCSND)
	$(OCAMLOPT) -o dead_code_analyzer.opt$(EXE) $(LIBS:.cma=.cmxa) $(SRCFST) deadLexiFi.ml $(SRCSND)
	cp -rf . ../build
	make clean

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj dead_code_analyzer.byt$(EXE) dead_code_analyzer.opt$(EXE)
