.PHONY: clean examples check

check:
	make -C check

examples:
	make -C examples

clean:
	make -C examples clean
	make -C check clean
	rm -rf build
