#Fix
- Function needed to match type of ungiven module signature
- Implementation without given interface needs to export all its values

#Testing
- \[ \] Test on big projects (OPAM, js_of_ocaml, coq, ...)      (currently testing on OPAM)
- \[X\] Test suite
	+ \[X\] Detect FP
	+ \[X\] Detect FN
	+ \[X\] Add some color and info (stats, ...)
- \[ \] Use different versions of OCaml (currently testing with 4.03.0+trunk)


#Note
- The 'UNUSED EXPORTED VALUES' may have both FN and FP (none known).
- The 'UNUSED VALUES' section may has FP; may have FN (none known).
- The 'OPTIONAL ARGUMENTS' section may have both FN and FP (none known).
- The 'CODING STYLE' section may have both FN and FP (none known).

ratio: *100%* succesful (over 89 expected results)
