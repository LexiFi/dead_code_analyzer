#FIX
- Conditional branching (if/match -> see cond_sel.ml)
- Clean code !!

#Testing
- \[ \] Test on big projects (OPAM, js_of_ocaml, coq, ...)
- \[X\] Test suite
	+ \[X\] Detect FP
	+ \[X\] Detect FN
	+ \[X\] Add some color and info (stats, ...)
- \[ \] Use different versions of OCaml (currently testing with 4.03.0+trunk)


#Note
- The 'UNUSED EXPORTED VALUES' section may have both FN and FP (none known).
- The 'UNUSED VALUES' section may have both FN and FP (none known).
- The 'OPTIONAL ARGUMENTS' section has FN; may have some unknown FP.
- The 'CODING STYLE' section may have both FN and FP (none known).

ratio: *94.94%* succesful (over 79 expected results)
