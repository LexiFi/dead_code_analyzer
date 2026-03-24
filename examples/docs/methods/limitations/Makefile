.PHONY: clean build

all: build

build:
	make -C alias build
	make -C factory_fun_indir build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C alias clean
	make -C factory_fun_indir clean

