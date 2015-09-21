#Testing
- \[ \] Test on big projects (OPAM, js_of_ocaml, coq, ...)
- \[ \] Test suite
	+ \[X\] Detect FP
	+ \[ \] Detect FN
	+ \[X\] Add some color and info (stats, ...)
- \[ \] Use different versions of OCaml (currently testing with 4.03.0+trunk)


#Note
- The 'UNUSED EXPORTED VALUES' section may have both FN and FP (none observed yet).
- The 'UNUSED VALUES' section has FP. No FN to report to this section yet
- The 'OPTIONAL ARGUMENTS' section has both FN and FP.
- The 'CODING STYLE' section may have both FN and FP (none observed yet).

ratio: *85.%* succesful (non exhaustive: not all known FN, may exist unknown FP)
