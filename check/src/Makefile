COMPFLAGS=-w +A-4-9-40-42 -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)

API_SRC=pretty_print.ml section.ml scores.ml
CHECK_SRC=$(API_SRC:.ml=.mli) $(API_SRC) check.mli check.ml
AGGR_SRC=$(API_SRC:.ml=.mli) $(API_SRC) aggregate.mli aggregate.ml

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
