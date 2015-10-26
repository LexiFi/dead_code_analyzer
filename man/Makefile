SRCS=../src/deadCode.mli
LIBS=-I +compiler-libs
DOCFLAGS=-man -man-mini

all:
	ocamldoc $(LIBS) $(DOCFLAGS) $(SRCS)
	mv *.3o man3/

clean:
	rm -f man3/*.3o
