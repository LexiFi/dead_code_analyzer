# dead_code_analyzer
Dead-code analyzer for OCaml (and more)

## Overview

This tool scans a compiled OCaml project and reports various warnings
about suspicious code:

 - Exported values never used.
   (The declaration can be dropped from the interface, and then from
   the implementation if there is no internal use -- which will be reportef
   by standard OCaml warnings).

 - Optional arguments for which either all call sites or none of them
   provide a value. (The argument can be made mandatory or dropped.)

 - Other stylistic issues:  patterns matching a value of type `unit`
   which are not `()` (typically, `_` or a variable);  let-binding
   `let () = ... in ...` (it's usually better to use sequencing);
   let-binding of the form `let x = ... in x` (the binding is useless);
   optional argument in argument's type: `val f: ... -> (... -> ?_:_ -> ...) -> ...`


The tool assumes that .mli files are compiled with -keep-locs and .ml
files with -bin-annot.  Exported values are collected by reading .cmi
files (obtained from explicit .mli interfaces).  References to such
values are collected by reading typed trees from .cmt files (obtained
by compiling .ml implementations with -bin-annot).

## Status

The project is used internally at LexiFi.

There has been no official release yet.


## Limitations

...


## Contact

This project was initiated by LexiFi (http://www.lexifi.com) and is part
of the SecurOCaml project.

Contact: alain.frisch@lexifi.com

