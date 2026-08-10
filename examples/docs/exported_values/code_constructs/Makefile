.PHONY: clean build

all: build

build:
	make -C function build
	make -C module build
	make -C functor build
	make -C modtyp build
	make -C modsig build
	make -C include build
	make -C incl_same_name build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C function clean
	make -C module clean
	make -C functor clean
	make -C modtyp clean
	make -C modsig clean
	make -C include clean
	make -C incl_same_name clean

