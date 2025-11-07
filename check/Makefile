.PHONY: clean prepare threshold-3-0.5 threshold-1 internal classic opt

EXT?=byt
SCENARIOS=threshold-3-0.5 threshold-1 internal classic
REDIRECT_SCENARIO?=""

all: aggregate.$(EXT)$(EXE)
	@echo "Running scenarios:"
	@# The 2nd echo in the loop below overlaps the 1st one for aesthetics purpose
	@for scenario in $(SCENARIOS); do \
		echo -n "$$scenario : In progress"; \
		make "$$scenario" REDIRECT_SCENARIO="> \"$$scenario.out\" 2> \"$$scenario.err\"" EXT="$(EXT)" > /dev/null 2>&1; \
		echo \\r"$$scenario : Done       "; \
	done
	./src/aggregate.$(EXT)$(EXE) $(SCENARIOS:=.out)

opt:
	make EXT=opt

.ONESHELL:
scenario: prepare check.$(EXT)$(EXE)
ifndef SCENARIO
	echo "Error: Please provide a SCENARIO name"
else
	TARGET="$(shell pwd)/$(SCENARIO)/$(SCENARIO).got"
	echo "$$TARGET"
	make -C ../examples run_dca DEADFLAGS="$(DEADFLAGS)" REDIRECT="> \"$$TARGET\""
	./src/check.$(EXT)$(EXE) ./"$(SCENARIO)"/"$(SCENARIO)".exp "$$TARGET" $(REDIRECT_SCENARIO)
ifndef DEADFLAGS
		echo "WARNING: No DEADFLAGS was provided"
endif
endif

classic:
	make scenario SCENARIO=$@ DEADFLAGS="-A"

internal:
	make scenario SCENARIO=$@ DEADFLAGS="-A --internal"

threshold-1:
	make scenario SCENARIO=$@ DEADFLAGS="-A --internal -E threshold:1 -M threshold:1 -T threshold:1"

threshold-3-0.5:
	make scenario SCENARIO=$@ DEADFLAGS="-A --internal -E threshold:3 -Oa both:3,0.5 -On both:3,0.5 -M threshold:3 -T threshold:3"

prepare:
	dune build ..
	make -C ../examples build > /dev/null 2>&1

check.$(EXT)$(EXE):
	make -C src $@

aggregate.$(EXT)$(EXE):
	make -C src $@

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out **.err */*.got
	make -C src clean
