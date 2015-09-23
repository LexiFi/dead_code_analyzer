COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=foo.mli foo.ml bar.ml baz.mli baz.ml qux.ml ifelse.ml match.ml matchopt.ml\
	opt-in-opt.ml useless-opt.ml unused-fn.ml fooFn.mli fooFn.ml bar-fn.ml\
	letIn.mli letIn.ml let-test.ml let-in.ml cond_sel.ml

all:
	$(OCAMLC) -c $(SRC)
	make -C opam
	../src/dead_code.byt $(DEADFLAGS) .

build:
	$(OCAMLC) -c $(SRC)
	make -C opam

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C opam clean
