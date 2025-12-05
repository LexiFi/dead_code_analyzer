# dead code analyzer
Dead-code analyzer for OCaml


[![Build Status](https://github.com/LexiFi/dead_code_analyzer/actions/workflows/workflow.yml/badge.svg?branch=master)](https://github.com/LexiFi/dead_code_analyzer/actions/workflows/workflow.yml)

## Overview

The tool assumes that **.mli** files are compiled with **-keep-locs** (activated
by default) and **.ml** files with **-bin-annot**.
Tracked elements of code are collected by reading **.cmi** and **.cmt** files.
Uses of such elements are collected by reading typed trees from **.cmt** files


This tool scans a compiled OCaml project and reports various warnings
about suspicious code:

- Exported values never used.
  The declaration can be dropped from the interface, and then from
  the implementation if there is no internal use -- which will be reported
  by standard OCaml warnings.\
  For more information, see the [Exported values documentation](docs/exported_values/EXPORTED_VALUES.md).

- Record fields and variant constructors never used.
  They can be dropped from the type definition and then the compiler will report
  the affected locations.\
  For more information, see the [Constructors/Record fields documentation](docs/fields_and_constructors/FIELDS_AND_CONSTRUCTORS.md).

- Exported public methods never used.
  They can be dropped from the class/object.\
  For more information, see the [Methods documentation](docs/methods/METHODS.md).

- Optional arguments for which either all call sites or none of them
  provide a value (other than `None`).
  They can be made mandatory or dropped.\
  For more information, see the [Optional arguments documentation](docs/optional_arguments/OPTIONAL_ARGUMENTS.md).

- Other stylistic issues:
  - patterns matching a value of type `unit` which are not `()` (typically, `_` or a variable);
  - let-binding to unit `let () = ... in ...` (it's usually better to use sequencing);
  - let-binding of the form `let x = ... in x` (the binding is useless);
  - optional argument in an argument's type: `val f: ... -> (... -> ?_:_ -> ...) -> ...`

  For more information, see the [Coding style documentation](coding_style/CODING_STYLE.md).

For more information, see the [User documentation](docs/USER_DOC.md)


## Requirements

- Currently tested and working on **OCaml 5.3**
- **dune >= 3.20**


## Install

### OPAM

`opam install dead_code_analyzer`

### Manual

1. Download the sources.
2. Build by running `make`
3. Run the `dead_code_analyzer` located in `_build/install/default/bin/`


## Usage

`dead_code_analyzer <options> <path>`

The given paths can be files and directories.\
For more information about the usage, use the `-help` option, or see the
[Usage documentation](docs/USAGE.md).


## Testing

`make check` runs each subset of the tests sequentially. In case of success,
they all run. Otherwise it stops at the first failing one and prints the diff
between the expected results and the current results.

To run a specific subset of the tests, run `make -C check <subset>`.

To get statistics about passing/failing tests run `make -C check stats`.

For each subset of the tests executed, a file `check/<subset>.out` is generated.
It contains the output of running the `dead_code_analyzer` on the tests with
annotations on failures:
- `Should not be detected` for false positives
- `Not detected` for false negatives

## Contribute

You are welcome to clone this repository, open issues and send us back pull requests.

Read `CONTRIBUTING.md` at the root of this directory for more information on how to contribute.


## Limitations

Tracking the optional arguments uses may consume a lot of memory.

Tracking the methods uses may consume a lot of memory.

For more limitations, see each [report section documentation](docs/USERDOC.md#Topics).


## Copyright

Source code is distributed under the conditions stated in file `License`


## Contact

This project was initiated by LexiFi (http://www.lexifi.com) and is part
of the SecurOCaml project.

Contact: alain.frisch@lexifi.com
