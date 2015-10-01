#FIX
- Module binding
- Conditional and opt args (see if_end.ml)
- Clean code !!

#Testing
- \[ \] Test on big projects (OPAM, js_of_ocaml, coq, ...)
- \[X\] Test suite
	+ \[X\] Detect FP
	+ \[X\] Detect FN
	+ \[X\] Add some color and info (stats, ...)
- \[ \] Use different versions of OCaml (currently testing with 4.03.0+trunk)


#Note
- The 'UNUSED EXPORTED VALUES' section has FP; may have FN (none known).
- The 'UNUSED VALUES' section has FN; may have FP (none known).
- The 'OPTIONAL ARGUMENTS' section has both FN and FP.
- The 'CODING STYLE' section may have both FN and FP (none known).

ratio: *91.21%* succesful (over 91 expected results)
