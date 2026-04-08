# Table of contents

+ [Coding style](#coding-style)
    + [Usage](#usage)
    + [Useless binding](#useless-binding)
        + [Example](#bind-example)
        + [Limitation](#bind-limitation)

# Coding style

This section relates to coding patterns. Although this is not related to dead
code, the tool has been reporting style issues almost since its conception
(commit [0adbc0d](https://github.com/LexiFi/dead_code_analyzer/commit/0adbc0d504830a01d64a701572b5e7cb3b29f03e)).
Tracking stylistic issues comes for "free" during the analysis and can be handy
when cleaning up a codebase so it was kept as part of the tool.

The analyzer only reports 4 different kinds of stylistic issues :
- `bind`: useless binding
- `opt`: parameter of arrow type expecting an optional argument
- `seq`: binding to unit instead of using sequence
- `unit`: binding unit to a name

## Usage

Stylistic issues are not reported by default.
Their reports can be activated by using the `--all` or `-S +all` command line
arguments.
They can be deactivated by using the `--nothing` or `-S -all` command line
arguments.
Each of the sylistic issue category can be selectively activated/decativated as
described in their respective sections.
For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

The report section looks like:
```
.> CODING STYLE:
===============
filepath:line: issue

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is `filepath:line: issue` with `filepath` the absolute
path to the file where the `issue` lies, `line` the line index in `filepath` at
which the `issue` is, and `issue` the description of the stylistic issue.
There can be any number of such lines.

The expected resolution depends on the reported issue category. This is
described in their respective sections.

## Useless binding

A.k.a `bind`.
This issue is different from unused variables or unused values (for more
details, see the  [Exported Values](../exported_values/EXPORTED_VALUES.md)
documentation).

This stylistic issue category can be selectively activated by using the
`-S +bind` command line argument.
It can be deactivated by using the `-S -bind` command line argument.

This category targets patterns of the form:
```OCaml
let x = ... in x
```
I.e. the variable is immediately returned.

The expected resolution is to remove the intermediate binding:
```Diff
- let x =
    ...
- in x
```

### Example <a name="bind-example"></a>

The reference files for this example are in the
[bind](../../examples/docs/coding_style/bind) directory.

The reference takes place in `/tmp/docs/coding_style`, which
is a copy of the [coding\_style](../../examples/docs/coding_style)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C bind build
```

The analysis command is :
```
make -C bind analyze
```

The compile + analyze command is :
```
make -C bind
```

Code:
```OCaml
(* bind.ml *)
let v =
  let interm = 42 in
  interm
```

Compile and analyze:
```
$ make -C bind
make: Entering directory '/tmp/docs/coding_style/bind'
ocamlopt -bin-annot bind.ml
dead_code_analyzer --nothing -S +bind .
Scanning files...
 [DONE]

.> CODING STYLE:
===============
/tmp/docs/coding_style/bind/bind.ml:3: let x = ... in x (=> useless binding)

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/coding_style/bind'
```

The analyzer reports a coding style issue in `tmp/docs/coding_style/bind/bind.ml`
at line `3`. The reported issue is `let x = ... in x (=> useless binding)`, aka
a `bind` issue.

The reported location points to `let interm = 42 in`. Removing the binding to
`interm` and replacing its use by its value (`42`) fixes the issue.

Code:
```OCaml
(* bind.ml *)
let v = 42
```

### Limitation <a name="bind-limitation"></a>

`bind` issues are always reported with the same content :
`let x = ... in x (=> useless binding)`. The name of the variable is not
adapted to fit the actual name found in code (`interm` in the example above).
