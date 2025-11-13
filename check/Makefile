.PHONY: clean prepare threshold-3-0.5 threshold-1 internal classic opt

#=(byt|opt) to select byte code or native compilation
EXT?=byt

SCENARIOS=threshold-3-0.5 threshold-1 internal classic
# rule names
REDIRECTED_SCENARIOS=$(SCENARIOS:%=redirected_%)
DIFF_SCENARIOS=$(SCENARIOS:%=diff_%)
# used by the scenario rule
TARGET=$(CURDIR)/$(SCENARIO)/$(SCENARIO).got
# used by the scenario rule and set in the redirected_scenario rule
REDIRECT_SCENARIO?=""

# Main rules

all: ci

ci: $(DIFF_SCENARIOS)

stats: $(REDIRECTED_SCENARIOS) aggregate.$(EXT)$(EXE)
	./src/aggregate.$(EXT)$(EXE) $(SCENARIOS:=.out)

opt:
	make EXT=opt

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out **.err */*.got
	make -C src clean

# Scenario rules

classic:
	make scenario SCENARIO=$@ DEADFLAGS="-A"

internal:
	make scenario SCENARIO=$@ DEADFLAGS="-A --internal"

threshold-1:
	make scenario SCENARIO=$@ DEADFLAGS="-A --internal -E threshold:1 -M threshold:1 -T threshold:1"

threshold-3-0.5:
	make scenario SCENARIO=$@ DEADFLAGS="-A --internal -E threshold:3 -Oa both:3,0.5 -On both:3,0.5 -M threshold:3 -T threshold:3"

$(REDIRECTED_SCENARIOS):
	make redirected_scenario SCENARIO=$(@:redirected_%=%)

$(DIFF_SCENARIOS):
	make diff_scenario SCENARIO=$(@:diff_%=%)

# Generic rules for scenarios. Require $(SCENARIO)

scenario: prepare check.$(EXT)$(EXE)
ifndef SCENARIO
	echo "Error: Please provide a SCENARIO name"
else
	make -C ../examples run_dca DEADFLAGS="$(DEADFLAGS)" REDIRECT="> $(TARGET)"
	./src/check.$(EXT)$(EXE) "./$(SCENARIO)/$(SCENARIO).exp" $(TARGET) $(REDIRECT_SCENARIO)
ifndef DEADFLAGS
		echo "WARNING: No DEADFLAGS was provided"
endif
endif

redirected_scenario:
ifndef SCENARIO
	echo "Error: Please provide a SCENARIO name"
else
	make "$(SCENARIO)" REDIRECT_SCENARIO="> \"$(SCENARIO).out\" 2>&1" EXT="$(EXT)"
endif

diff_scenario:
ifndef SCENARIO
	echo "Error: Please provide a SCENARIO name"
else
	make redirected_scenario SCENARIO="$(SCENARIO)" EXT="$(EXT)"
	diff -s "./$(SCENARIO)/$(SCENARIO).ref" "$(SCENARIO).out"
endif

# Helpers. A user can call them by hand but it is unlikely to happen

prepare:
	dune build ..
	make -C ../examples build > /dev/null 2>&1

check.$(EXT)$(EXE):
	make -C src $@

aggregate.$(EXT)$(EXE):
	make -C src $@
