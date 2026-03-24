.PHONY: clean build

all: build

build:
	make -C class build
	make -C polymorphic_class build
	make -C constructor build
	make -C class_type build
	make -C inheritance build
	make -C immediate_object build
	make -C factory_fun build
	make -C object_type build
	make -C coercion build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C class clean
	make -C polymorphic_class clean
	make -C constructor clean
	make -C class_type clean
	make -C inheritance clean
	make -C immediate_object clean
	make -C factory_fun clean
	make -C object_type clean
	make -C coercion clean

