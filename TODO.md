# Waiting
- better doc
- handle module types better
- improve optional arguments memory cost
- more precise on optional arguments (track dependencies)
- handle branching (for methods and opt args)
- make check/\*.ml Str independent
- improve methods (e.g. examples/obj/fun_obj_param.ml)
- improve performances


# Testing
- \[X\] Test suite
- \[ \] Test on big projects: see directory `results`
- \[ \] Create more precise tests
- \[ \] Create tests for all options


# Note
- The `UNUSED EXPORTED VALUES` section may have both FN and FP (none known).
- The `UNUSED METHODS` section may have both FN and FP.
  It may consume a lot of memory to compute.
- The `UNUSED CONSTRUCTORS/RECORD FIELDS` section may have both FN and FP (none known).
- The `OPTIONAL ARGUMENTS` sections may have both FN and FP.
  It may consume a lot of memory to compute.
- The `CODING STYLE` section may have both FN and FP (none known).

ratio: **100%** succesful (over 255 unique expected results)
