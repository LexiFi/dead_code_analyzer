.PHONY: clean build

all: build

build:
	make -C immediate_object build
	make -C class build
	make -C class_type build
	make -C inheritance build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C immediate_object clean
	make -C class clean
	make -C class_type clean
	make -C inheritance clean

