.PHONY: clean build

all: build

build:
	make -C exported_values
	make -C methods
	make -C fields_and_constructors
	make -C optional_arguments

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C exported_values clean
	make -C methods clean
	make -C fields_and_constructors clean
	make -C optional_arguments clean

