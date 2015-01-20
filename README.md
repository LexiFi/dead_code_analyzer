# dead_code_analyzer
Dead-code analyzer for OCaml

This tool reports values exported by .mli files but never used in
any other module.  It assumes that .mli files are compiled with
-keep-locs and .ml files with -bin-annot.

Exported values are collected by reading .cmi files (obtained from
explicit .mli interfaces).  References to such values are collected by
reading typed trees from .cmt files (obtained by compiling .ml
implementations with -bin-annot).

The tool also analyses calls to functions with optional arguments.
If, for a given function and one of its optional argument, all call
sites (resp. none of them) provide a value for the argument, then this
argument is reported.
