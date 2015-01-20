# dead_code_analyzer
Dead-code analyzer for OCaml

This tool reports values exported by .mli files but never used in
any other module.  It assumes that .mli files are compiled with
-keep-locs and .ml files with -bin-annot.
