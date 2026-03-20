.PHONY: clean build

all: build

build:
	make -C immediate_object build
	make -C class build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C immediate_object clean
	make -C class clean

