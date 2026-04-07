.PHONY: clean build

all: build

build:
	make -C total_app build
	make -C partial_app build
	make -C internal_app build
	make -C external_app build
	make -C intext_app build

clean:
	rm -f *~ *.cm* *.o *.obj
	make -C total_app clean
	make -C partial_app clean
	make -C internal_app clean
	make -C external_app clean
	make -C intext_app clean
