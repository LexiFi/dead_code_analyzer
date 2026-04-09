.PHONY: clean build

all: build

build:
	make -C bind build
	make -C opt build
	make -C seq build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C bind clean
	make -C opt clean
	make -C seq clean

