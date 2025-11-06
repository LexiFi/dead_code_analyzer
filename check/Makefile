.PHONY: clean prepare threshold-3-0.5 threshold-1 internal classic opt

TARGET?=res.out
EXT?=byt
SCENARIOS=threshold-3-0.5 threshold-1 internal classic
REDIRECT_SCENARIO?=""

all: aggregate.$(EXT)$(EXE)
	@echo "Running scenarios:"
	@# The 2nd echo in the loop below overlaps the 1st one for aesthetics purpose
	@for scenario in $(SCENARIOS); do \
		echo -n "$$scenario : In progress"; \
		make $$scenario REDIRECT_SCENARIO="> $$scenario.out 2> $$scenario.err" EXT=$(EXT) > /dev/null 2>&1; \
		echo \\r"$$scenario : Done       "; \
	done
	./src/aggregate.$(EXT)$(EXE) $(SCENARIOS:=.out)

opt:
	make EXT=opt

classic: prepare check.$(EXT)$(EXE)
	make -C ../examples run_dca DEADFLAGS="-A" REDIRECT="> $(TARGET)"
	mv ../examples/$(TARGET) .
	./src/check.$(EXT)$(EXE) ./classic/classic.exp $(TARGET) $(REDIRECT_SCENARIO)

internal: prepare check.$(EXT)$(EXE)
	make -C ../examples run_dca DEADFLAGS="-A --internal" REDIRECT="> $(TARGET)"
	mv ../examples/$(TARGET) .
	./src/check.$(EXT)$(EXE) ./internal/internal.exp $(TARGET) $(REDIRECT_SCENARIO)

threshold-1: prepare check.$(EXT)$(EXE)
	make -C ../examples run_dca DEADFLAGS="-A --internal -E threshold:1 -M threshold:1 -T threshold:1" REDIRECT="> $(TARGET)"
	mv ../examples/$(TARGET) .
	./src/check.$(EXT)$(EXE) ./threshold-1/threshold-1.exp $(TARGET) $(REDIRECT_SCENARIO)

threshold-3-0.5: prepare check.$(EXT)$(EXE)
	make -C ../examples run_dca DEADFLAGS="-A --internal -E threshold:3 -Oa both:3,0.5 -On both:3,0.5 -M threshold:3 -T threshold:3" REDIRECT="> $(TARGET)"
	mv ../examples/$(TARGET) .
	./src/check.$(EXT)$(EXE) ./threshold-3-0.5/threshold-3-0.5.exp $(TARGET) $(REDIRECT_SCENARIO)

prepare:
	dune build ..
	make -C ../examples build > /dev/null 2>&1

check.$(EXT)$(EXE):
	make -C src $@

aggregate.$(EXT)$(EXE):
	make -C src $@

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out **.err
	make -C src clean
