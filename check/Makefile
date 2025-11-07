.PHONY: clean prepare threshold-3-0.5 threshold-1 internal classic opt

EXT?=byt
SCENARIOS=threshold-3-0.5 threshold-1 internal classic
REDIRECTED_SCENARIOS=$(SCENARIOS:%=redirected_%) # rule names
REDIRECT_SCENARIO?="" # used by the scenario rule and set in the redirected_scenario rule
TARGET=$(CURDIR)/$(SCENARIO)/$(SCENARIO).got # used by the scenario rule

all: $(REDIRECTED_SCENARIOS) aggregate.$(EXT)$(EXE)
	./src/aggregate.$(EXT)$(EXE) $(SCENARIOS:=.out)

opt:
	make EXT=opt

scenario: prepare check.$(EXT)$(EXE)
ifndef SCENARIO
	echo "Error: Please provide a SCENARIO name"
else
	echo "$(TARGET)"
	make -C ../examples run_dca DEADFLAGS="$(DEADFLAGS)" REDIRECT="> $(TARGET)"
	echo $(CURDIR)
	./src/check.$(EXT)$(EXE) "./$(SCENARIO)/$(SCENARIO).exp" $(TARGET) $(REDIRECT_SCENARIO)
ifndef DEADFLAGS
		echo "WARNING: No DEADFLAGS was provided"
endif
endif

redirected_scenario:
ifndef SCENARIO
	echo "Error: Please provide a SCENARIO name"
else
	make "$(SCENARIO)" REDIRECT_SCENARIO="> \"$(SCENARIO).out\" 2> \"$(SCENARIO).err\"" EXT="$(EXT)" > /dev/null 2>&1
endif

$(REDIRECTED_SCENARIOS):
	make redirected_scenario SCENARIO=$(@:redirected_%=%)

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
