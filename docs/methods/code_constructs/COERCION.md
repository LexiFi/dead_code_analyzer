The reference files for this example are in the
[coercion](../../../examples/docs/methods/code_constructs/coercion) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C coercion build
```

The analysis command is :
```
make -C coercion analyze
```

The compile + analyze command is :
```
make -C coercion
```

## First run

Code:
```OCaml
(* coercion_lib.mli *)
val obj :
  < used_by_requirement : unit
  ; unused : unit
  >
```
```OCaml
(* coercion_lib.ml *)
let obj =
  object
    method used_by_requirement = ()
    method unused = ()
  end
```
```OCaml
(* coercion_bin.ml *)
open Coercion_lib

let () =
  let _coerce = (obj :> < used_by_requirement : unit >) in
  ()
```

Before looking at the analysis results, let's look at the code.

There is 1 object : `Coercion_lib.obj`, which defined 2 zmthods :
`used_by_requirement`, and `unused`. Neither of them is explicitly referenced,
and the object is only manipulated through a coercion. The coercion produces an
alias to `obj` named `_coerce`, which only exposes the method
`used_by_requirement`. Because the method is required to exists in `obj` for the
coercion, then the analyzer effectively considers it as used by requirement.

Compile and analyze:
```
$ make -C coercion
make: Entering directory '/tmp/docs/methods/code_constructs/coercion'
ocamlopt -bin-annot coercion_lib.mli coercion_lib.ml coercion_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/coercion/coercion_lib.mli:2: obj#unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/coercion'
```

As expected, the anlyzer reports `obj#unsed` as unused and not
`obj#used_by_requirement`. The unused method can be removed from the `.mli` and
the `.ml`. Our work here is done.
