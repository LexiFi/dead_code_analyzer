.PHONY: clean build

all: build

build:
	make -C polymorphic_type build
	make -C polymorphic_variant build
	make -C gadt build
	make -C inline_record build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C polymorphic_type clean
	make -C polymorphic_variant clean
	make -C gadt clean
	make -C inline_record clean
