COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=refFn.mli refFn.ml ref-fn.ml ref-opt.ml anonFn.mli anonFn.ml anon-call.ml\
	anonFn2.mli anonFn2.ml anon-call2.ml alias-opt.ml ref-opt.ml fn-arg.ml\
	partial.ml match-opt.ml mod.mli mod.ml sub_mod.mli sub_mod.ml

all:
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
