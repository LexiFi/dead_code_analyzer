.PHONY: clean build

all: build

build:
	make -C total_app build
	make -C partial_app build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C total_app clean
	make -C partial_app clean

