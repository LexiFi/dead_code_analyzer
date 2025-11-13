# =(byt|opt) to select byte code or native compilation
EXT?=byt

SCENARIOS:=threshold-3-0.5 threshold-1 internal classic
# rule names
REDIRECTED_SCENARIOS:=$(SCENARIOS:%=redirected_%)
DIFF_SCENARIOS:=$(SCENARIOS:%=diff_%)
# used by the scenario rule
TARGET=$(CURDIR)/$(SCENARIO)/$(SCENARIO).got
# used by the scenario rule and set in the redirected_scenario rule
REDIRECT_SCENARIO?=""

# Main rules

.PHONY: all ci stats clean opt

all: ci

ci: $(DIFF_SCENARIOS)

stats: $(REDIRECTED_SCENARIOS) aggregate.$(EXT)$(EXE)
	./src/aggregate.$(EXT)$(EXE) $(SCENARIOS:=.out)

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.byt$(EXE) *.opt$(EXE) **.out **.err */*.got
	make -C src clean

opt:
	make EXT=opt

# Scenario rules

.PHONY: $(SCENARIOS) $(REDIRECTED_SCENARIOS) $(DIFF_SCENARIOS)

classic:
	make scenario EXT=$(EXT) SCENARIO=$@ DEADFLAGS="-A"

internal:
	make scenario EXT=$(EXT) SCENARIO=$@ DEADFLAGS="-A --internal"

threshold-1:
	make scenario EXT=$(EXT) SCENARIO=$@ DEADFLAGS="-A --internal -E threshold:1 -M threshold:1 -T threshold:1"

threshold-3-0.5:
	make scenario EXT=$(EXT) SCENARIO=$@ DEADFLAGS="-A --internal -E threshold:3 -Oa both:3,0.5 -On both:3,0.5 -M threshold:3 -T threshold:3"

$(REDIRECTED_SCENARIOS):
	SCENARIO=$(@:redirected_%=%); \
	make "$$SCENARIO" REDIRECT_SCENARIO="> \"$$SCENARIO.out\" 2>&1" EXT=$(EXT)

$(DIFF_SCENARIOS):
	make $(@:diff_%=redirected_%) EXT=$(EXT)
	SCENARIO=$(@:diff_%=%); \
	diff -s "./$$SCENARIO/$$SCENARIO.ref" "$$SCENARIO.out"

# Generic rule for scenarios. Require $(SCENARIO)

.PHONY: scenario

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

# Helpers. A user can call them by hand but it is unlikely to happen

.PHONY: prepare

prepare:
	dune build ..
	make -C ../examples build > /dev/null 2>&1

check.$(EXT)$(EXE):
	make -C src $@

aggregate.$(EXT)$(EXE):
	make -C src $@
