COMPFLAGS=-w +A-4-9-40-42 -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

CHECK_SRC=check.ml
AGGR_SRC=aggregate.ml

check.byt$(EXE): $(CHECK_SRC)
	$(OCAMLC) -o $@ $^

check.opt$(EXE): $(CHECK_SRC)
	$(OCAMLOPT) -o $@ $^

aggregate.byt$(EXE): $(AGGR_SRC)
	$(OCAMLC) -o $@ $^

aggregate.opt$(EXE): $(AGGR_SRC)
	$(OCAMLOPT) -o $@ $^

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out
