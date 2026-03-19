.PHONY: clean build

all: build

build:
	make -C exported_values
	make -C methods

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C exported_values clean
	make -C methods clean

