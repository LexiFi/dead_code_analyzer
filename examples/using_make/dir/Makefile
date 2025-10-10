COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=refFn.mli refFn.ml ref_fn.ml ref_opt.ml anonFn.mli anonFn.ml anon_call.ml\
	anonFn2.mli anonFn2.ml anon_call2.ml alias_opt.ml fn_arg.ml\
	partial.ml match_opt.ml mod.mli mod.ml sub_mod.mli sub_mod.ml\
	hidden_opt_use.ml matchopt.ml
CMTI=$(filter %.cmi,$(SRC:.ml=.cmi)) $(filter %.cmt,$(SRC:.ml=.cmt))

all: $(CMTI)

$(CMTI): $(SRC)
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
