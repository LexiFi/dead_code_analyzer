SRC:=definitions.ml \
     via_explicit_equations.ml \
     via_hidden_equations.mli via_hidden_equations.ml \
     via_include.mli via_include.ml \
     via_module_alias.mli via_module_alias.ml \
     all_internal.mli all_internal.ml \
     equal_types.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	~/proj/dead_code_analyzer/_build/install/default/bin/dead_code_analyzer --nothing -T all .

clean:
	rm -f *.cm* *.o a.out
