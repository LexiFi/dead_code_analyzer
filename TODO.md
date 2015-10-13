#Fix
- Stop assuming each basename should be unique
- Clean code!!


#Testing
- \[ \] Test on big projects
	+ \[X\] js_of_ocaml: exported values: 640 values; no FP
	+ \[X\] js_of_ocaml: opt args never used: no FP
	+ \[X\] OPAM: exported values: 234 values reported; no FP; 32 FN detected
	+ \[X\] OPAM: opt args never used: no FP
- \[X\] Test suite
	+ \[X\] Detect FP
	+ \[X\] Detect FN
	+ \[X\] Add some color and info (stats, ...)


#Note
- The 'UNUSED EXPORTED VALUES' section may has FN; may have FP (none known).
- The 'OPTIONAL ARGUMENTS' section has both FN and FP (due to 'ref' usage).
- The 'CODING STYLE' section may have both FN and FP (none known).

ratio: *95.74%* succesful (over 94 expected results)
