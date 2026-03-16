.PHONY: clean build

all: build

build:
	make -C sigincl build
	make -C incl_same_name build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C sigincl clean
	make -C incl_same_name clean
