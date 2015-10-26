include $(shell ocamlc -where)/Makefile.config

LIBS=str.cma
COMPFLAGS=-w +A-4-9-40-42 -bin-annot -keep-locs -g
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=check.ml
TARGET=res.out
DEADFLAGS=-A

all: check.byt$(EXE)
	../src/deadCode.byt$(EXE) $(DEADFLAGS) ../examples > $(TARGET)
	./check.byt$(EXE) . $(TARGET)

opt: check.opt$(EXE)
	../src/deadCode.opt$(EXE) $(DEADFLAGS) ../examples > $(TARGET)
	./check.opt$(EXE) . $(TARGET)

check.byt$(EXE): $(SRC)
	$(OCAMLC) $(LIBS) $(SRC) -o $@

check.opt$(EXE): $(SRC)
	$(OCAMLC) $(LIBS) $(SRC) -o $@

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj check.byt$(EXE) check.opt$(EXE) *.out
