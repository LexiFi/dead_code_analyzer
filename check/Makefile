
TARGET=res.out
EXT=byt
SCENARIOS=threshold-3-0.5 threshold-1 internal classic

all: aggregate.$(EXT)$(EXE)
	@echo "Running scenarios:"
	@# The 2nd echo in the loop below overlaps the 1st one for aesthetics purpose
	@for scenario in $(SCENARIOS); do \
		echo -n "$$scenario : In progress"; \
		make $$scenario > $$scenario.out 2>&1 EXT=$(EXT); \
		echo \\r"$$scenario : Done       "; \
	done
	./src/aggregate.$(EXT)$(EXE) $(SCENARIOS:=.out)

opt:
	make EXT=opt

internal: prepare check.$(EXT)$(EXE)
	../_build/install/default/bin/dead_code_analyzer -A --internal ../examples > $(TARGET)
	./src/check.$(EXT)$(EXE) ./internal $(TARGET)

classic: prepare check.$(EXT)$(EXE)
	../_build/install/default/bin/dead_code_analyzer -A ../examples > $(TARGET)
	./src/check.$(EXT)$(EXE) ./classic $(TARGET)

threshold-1: prepare check.$(EXT)$(EXE)
	../_build/install/default/bin/dead_code_analyzer -A --internal -E threshold:1 -M threshold:1 -T threshold:1 ../examples > $(TARGET)
	./src/check.$(EXT)$(EXE) ./threshold-1 $(TARGET)

threshold-3-0.5: prepare check.$(EXT)$(EXE)
	../_build/install/default/bin/dead_code_analyzer -A --internal -E threshold:3 -Oa both:3,0.5 -On both:3,0.5 -M threshold:3 -T threshold:3 ../examples > $(TARGET)
	./src/check.$(EXT)$(EXE) ./threshold-3-0.5 $(TARGET)

prepare:
	dune build ..
	make -C ../examples build > /dev/null 2>&1

check.$(EXT)$(EXE):
	make -C src $@

aggregate.$(EXT)$(EXE):
	make -C src $@

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out
	make -C src clean
