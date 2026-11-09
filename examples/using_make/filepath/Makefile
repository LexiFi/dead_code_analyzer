.PHONY: clean build

all: build

build:
	make -C absolute
	make -C relative

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C absolute clean
	make -C relative clean

