.PHONY: clean build

COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=foo.mli foo.ml bar.ml baz.mli baz.ml qux.ml ifelse.ml match.ml matchopt.ml\
	opt_in_opt.ml useless_opt.ml unused_fn.ml fooFn.mli fooFn.ml bar_fn.ml\
	letIn.mli letIn.ml let_test.ml let_in.ml cond_sel.ml if_end.ml\
	mod_alias.mli mod_alias.ml functor.mli functor.ml partial_opt.ml\
	record.mli record.ml variant.mli variant.ml exported.mli exported.ml

all: build

build:
	$(OCAMLC) -c $(SRC)
	make -C dir
	make -C obj
	make -C opt
	make -C advanced

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C dir clean
	make -C obj clean
	make -C opt clean
	make -C advanced clean

