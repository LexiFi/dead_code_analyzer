.PHONY: clean build

all: build

build:
	make -C hello_world
	make -C code_constructs
	make -C limitations

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C hello_world clean
	make -C code_constructs clean
	make -C limitations clean

