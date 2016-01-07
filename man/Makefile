SRCS=../src/deadCode.mli
LIBS=-I +compiler-libs
DOCFLAGS=-man -man-mini

all:
	ocamldoc $(LIBS) $(DOCFLAGS) $(SRCS)
	mv *.3o man3/
	mv man3/DeadCode.3o man3/dead_code_analyzer.3o

clean:
	rm -f man3/*.3o
