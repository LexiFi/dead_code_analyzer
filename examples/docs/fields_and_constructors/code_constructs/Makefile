.PHONY: clean build

all: build

build:
	make -C polymorphic_type build
	make -C polymorphic_variant build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C polymorphic_type clean
	make -C polymorphic_variant clean
