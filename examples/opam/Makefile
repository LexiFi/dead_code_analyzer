COMPFLAGS=-w +A-4-9-40-42 -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=refFn.mli refFn.ml ref-fn.ml ref-opt.ml anonFn.mli anonFn.ml anon-call.ml\
	anonFn2.mli anonFn2.ml anon-call2.ml alias-opt.ml ref-opt.ml fn-arg.ml\
	partial.ml match-opt.ml

all:
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
