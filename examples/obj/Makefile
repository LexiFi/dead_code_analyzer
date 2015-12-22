COMPFLAGS=-w +A -I +compiler-libs -bin-annot -keep-locs
OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
SRC=class.mli class.ml inher.mli inher.ml fooCl.mli fooCl.ml barCl.mli barCl.ml\
overr.mli overr.ml named.mli named.ml class_type.mli class_type.ml coerce.mli coerce.ml\
obj_inher.ml object.ml constraint.ml depend.ml fun_class.ml fun_obj_param.ml

all:
	$(OCAMLC) -c $(SRC)

clean:
	rm -f *~ *.cm* *.o *.obj
