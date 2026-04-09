SRC:=hof_bin.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -Oa all -On all .

clean:
	rm -f *.cm* *.o a.out
