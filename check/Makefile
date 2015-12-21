include $(shell ocamlc -where)/Makefile.config

LIBS=str.cma
COMPFLAGS=-w +A-4-9-40-42 -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=check.ml
TARGET=res.out
EXT=byt

all: aggr.$(EXT)$(EXE)
	make internal > internal.out 2>&1 EXT=$(EXT)
	make classic > classic.out 2>&1 EXT=$(EXT)
	make threshold-1 > threshold-1.out 2>&1 EXT=$(EXT)
	make threshold-3-0.5 > threshold-3-0.5.out 2>&1 EXT=$(EXT)
	./aggr.$(EXT)$(EXE) threshold-3-0.5.out threshold-1.out internal.out classic.out

opt:
	make EXT=opt

internal: prepare check.$(EXT)$(EXE)
	../build/deadCode.$(EXT)$(EXE) -A --internal ../examples > $(TARGET)
	./check.$(EXT)$(EXE) ./internal $(TARGET)

classic: prepare check.$(EXT)$(EXE)
	../build/deadCode.$(EXT)$(EXE) -A ../examples > $(TARGET)
	./check.$(EXT)$(EXE) ./classic $(TARGET)

threshold-1: prepare check.$(EXT)$(EXE)
	../build/deadCode.$(EXT)$(EXE) -A --internal -E threshold:1 -M threshold:1 -T threshold:1 ../examples > $(TARGET)
	./check.$(EXT)$(EXE) ./threshold-1 $(TARGET)

threshold-3-0.5: prepare check.$(EXT)$(EXE)
	../build/deadCode.$(EXT)$(EXE) -A --internal -E threshold:3 -Oa both:3,0.5 -On both:3,0.5 -M threshold:3 -T threshold:3 ../examples > $(TARGET)
	./check.$(EXT)$(EXE) ./threshold-3-0.5 $(TARGET)

prepare:
	make -C ../src deadCode.$(EXT)$(EXE) > /dev/null 2>&1
	make -C ../examples build > /dev/null 2>&1

check.byt$(EXE): $(SRC)
	$(OCAMLC) -o $@ $(LIBS) $^

check.opt$(EXE): $(SRC)
	$(OCAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $^

aggr.byt$(EXE): aggregate.ml
	$(OCAMLC) -o $@ $^

aggr.opt$(EXE): aggregate.ml
	$(OCAMLOPT) -o $@ $^

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out
