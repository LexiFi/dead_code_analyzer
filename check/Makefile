include $(shell ocamlc -where)/Makefile.config

LIBS=str.cma
COMPFLAGS=-w +A-4-9-40-42 -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=check.ml
TARGET=res.out

all: aggr.byt$(EXE)
	make internal > internal.out 2>&1
	make classic > classic.out 2>&1
	make threshold-1 > threshold-1.out 2>&1
	make threshold-3-0.5 > threshold-3-0.5.out 2>&1
	./aggr.byt$(EXE) threshold-3-0.5.out threshold-1.out internal.out classic.out

internal: prepare check.byt$(EXE)
	../src/deadCode.byt$(EXE) -A --internal ../examples > $(TARGET)
	./check.byt$(EXE) ./internal $(TARGET)

classic: prepare check.byt$(EXE)
	../src/deadCode.byt$(EXE) -A ../examples > $(TARGET)
	./check.byt$(EXE) ./classic $(TARGET)

threshold-1: prepare check.byt$(EXE)
	../src/deadCode.byt$(EXE) -A --internal --threshold +1 ../examples > $(TARGET)
	./check.byt$(EXE) ./threshold-1 $(TARGET)

threshold-3-0.5: prepare check.byt$(EXE)
	../src/deadCode.byt$(EXE) -A --internal --threshold +3+0.5+both ../examples > $(TARGET)
	./check.byt$(EXE) ./threshold-3-0.5 $(TARGET)

prepare:
	make -C .. examples > /dev/null 2>&1

opt: check.opt$(EXE)
	../src/deadCode.opt$(EXE) $(DEADFLAGS) ../examples > $(TARGET)
	./check.opt$(EXE) . $(TARGET)

check.byt$(EXE): $(SRC)
	$(OCAMLC) $(LIBS) $(SRC) -o $@

check.opt$(EXE): $(SRC)
	$(OCAMLC) $(LIBS) $(SRC) -o $@

aggr.byt$(EXE): aggregate.ml
	$(OCAMLC) $^ -o $@

aggr.opt$(EXE): aggregate.ml
	$(OCAMLC) $^ -o $@

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out
