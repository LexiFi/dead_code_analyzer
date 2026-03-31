The reference files for this example are in the
[polymorphic\_variant](../../../examples/docs/fields_and_constructors/code_constructs/polymorphic_variant) directory.

The reference takes place in `/tmp/docs/fields_and_constructors/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/fields_and_constructors/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C polymorphic_variant build
```

The analysis command is :
```
make -C polymorphic_variant analyze
```

The compile + analyze command is :
```
make -C polymorphic_variant
```

> [!IMPORTANT]
> **LIMITATION**
>
> The analyzer ignores polymorphic variant constructors.

## First run

Code :
```OCaml
(* polymorphic_variant_lib.mli *)
type poly = [`Int of int | `Float of float]

val poly_of_int : int -> [> `Int of int]

val float_opt_of_poly : poly -> float option
```
```OCaml
(* polymorphic_variant_lib.ml *)
type poly = [`Int of int | `Float of float]

let poly_of_int x = `Int x

let float_opt_of_poly = function
  | `Float f -> Some f
  | _ -> None
```
```OCaml
(* polymorphic_variant_bin.ml *)
let () =
  let open Polymorphic_variant_lib in
  let x = 0 in
  let poly = poly_of_int x in
  let f = float_opt_of_poly poly in
  assert (f = None)
```

Before looking at the analysis results, let's look at the code.

The `Polymorphic_variant_lib` uses 2 polymorphic variant constructors : `` `Int ``
and `` `Float ``. The first one is built in `poly_of_int`. The second one is
matched on in `float_opt_of_poly`.
Following the classical variant constructor semantics, `` `Int `` is used and
`` `Float `` is not.


One could expect the analyzer to report `` `Float `` as unused. However, the
analyzer does not report unused polymorphic variant constructors.
Unlike regular variant constructors, polymorphic variant constructors do not
belong to any specific type. That flexibility is their purpose. Currently,
reporting a constructor requires that it is associated with a single type.

The compiler also does not warn on unused polymorphic variant constructors.

Compile and analyze :
```
$ make -C polymorphic_variant
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_variant'
ocamlopt -w +37 -bin-annot polymorphic_variant_lib.mli polymorphic_variant_lib.ml polymorphic_variant_bin.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_variant'
```

As explained, nothing is reported. Our work here is done.
