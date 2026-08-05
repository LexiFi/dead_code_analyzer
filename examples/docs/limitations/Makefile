.PHONY: clean build

all: build

build:
	make -C modtype build
	make -C modtype_of build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C modtype clean
	make -C modtype_of clean
