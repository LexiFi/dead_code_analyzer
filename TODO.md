#Fix
- Stop assuming each basename should be unique
- Clean code!!


#Testing
- \[ \] Test on big projects
	+ \[X\] js_of_ocaml: exported values: 571 values; no FP
	+ \[X\] js_of_ocaml: opt args never used: no FP
	+ \[X\] OPAM: exported values: 235 values reported; no FP
	+ \[X\] OPAM: opt args never used: no FP
- \[X\] Test suite
	+ \[X\] Detect FP
	+ \[X\] Detect FN
	+ \[X\] Add some color and info (stats, ...)


#Note
- The 'UNUSED EXPORTED VALUES' section may have both FN and FP (none known).
- The 'OPTIONAL ARGUMENTS' section has both FN and FP (due to 'ref' usage).
- The 'CODING STYLE' section may have both FN and FP (none known).

ratio: *96.81%* succesful (over 94 expected results)
