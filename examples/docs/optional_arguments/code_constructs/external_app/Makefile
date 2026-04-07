SRC:=external_app_lib.mli external_app_lib.ml external_app_bin.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -Oa all -On all .

clean:
	rm -f *.cm* *.o a.out
