#FIX
- Conditional branching (if/match)
- Partial application as arg
- Clean code

#Testing
- \[ \] Test on big projects (OPAM, js_of_ocaml, coq, ...)
- \[X\] Test suite
	+ \[X\] Detect FP
	+ \[X\] Detect FN
	+ \[X\] Add some color and info (stats, ...)
- \[ \] Use different versions of OCaml (currently testing with 4.03.0+trunk)


#Note
- The 'UNUSED EXPORTED VALUES' section may have both FN and FP (none observed yet).
- The 'UNUSED VALUES' section has FN; may have some unknown FP.
- The 'OPTIONAL ARGUMENTS' section has both FN and FP.
- The 'CODING STYLE' section may have both FN and FP (none observed yet).

ratio: *85.333%* succesful (over 75 expected results)
