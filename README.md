# dead code analyzer
Dead-code analyzer for OCaml


## Overview

The tool assumes that .mli files are compiled with -keep-locs and .ml
files with -bin-annot.  Exported values are collected by reading .cmi or .cmt
files (depending on the existence of an explicit .mli interface).
References to such values are collected by reading typed trees from .cmt files


This tool scans a compiled OCaml project and reports various warnings
about suspicious code:

 - Exported values never used.
   (The declaration can be dropped from the interface, and then from
   the implementation if there is no internal use -- which will be reported
   by standard OCaml warnings).

 - Types fields and constructors never used. (Can be dropped from the type)

 - Class fields never used. (Can be dropped from the signature)

 - Optional arguments for which either all call sites or none of them
   provide a value (other than `None`. (The argument can be made mandatory or dropped.)

 - Other stylistic issues:  patterns matching a value of type `unit`
   which are not `()` (typically, `_` or a variable);  let-binding
   `let () = ... in ...` (it's usually better to use sequencing);
   let-binding of the form `let x = ... in x` (the binding is useless);
   optional argument in argument's type: `val f: ... -> (... -> ?_:_ -> ...) -> ...`


## Requirements

- Currently tested and working on OCaml 4.03.0+trunk


## Install and Use

1. Download the sources.
2. Run `make` to generate the `deadCode.byt` bytecode executable file;
Run `make opt` to generate the `deadCode.opt` native-code executable file.
Both will be produced in the `build` directory.
3. Execute the analyzer on the desired sources.

For more information about the usage, use the *-help* option.


## Testing

To run the tests use `make check`.
For each subset of the tests a file `<subset>.out` is generated, containg the output
of the execution on it.

To run a subset of the tests call `make -C check <subset>`.


## Documentation

Running `make doc` will produce the documentation as html files in the `doc` directory.

Running `make man` will produce documentation as man pages in the `man/man3` directory.


## Contribute

You are welcome to clone this repository and send us back pull requests.

Please verify that your modifications are valid:
- Add tests for your work in the `examples` directory.
- Update the expected results found in the `check` directory according to your changes
and common sense (i.e. do not expect a result that is obviously wrong)
- Run `make check` and verify nothing is broken
- Compare your results to the one reported in the `results` directory
for better verification (some cases may not have their corresponding simplified test written yet)


## Status

The project is used internally at LexiFi.

There has been no official release yet.


## Limitations

Tracking the optional arguments uses may consume a lot of memory.


## Contact

This project was initiated by LexiFi (http://www.lexifi.com) and is part
of the SecurOCaml project.

Contact: alain.frisch@lexifi.com
