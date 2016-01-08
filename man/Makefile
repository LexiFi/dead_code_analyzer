SRCS=../src/deadCode.mli
LIBS=-I +compiler-libs
DOCFLAGS=-man -man-mini

all:
	ocamldoc $(LIBS) $(DOCFLAGS) $(SRCS)
	rm -rf man3/
	mkdir man3
	mv *.3o man3/
	mv man3/DeadCode.3o man3/dead_code_analyzer.3o

clean:
	rm -rf man3
