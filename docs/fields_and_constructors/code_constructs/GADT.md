The reference files for this example are in the
[gadt](../../../examples/docs/fields_and_constructors/code_constructs/gadt) directory.

The reference takes place in `/tmp/docs/fields_and_constructors/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/fields_and_constructors/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C gadt build
```

The analysis command is :
```
make -C gadt analyze
```

The compile + analyze command is :
```
make -C gadt
```

## First run

Code :
```OCaml
(* gadt_lib.mli *)
type _ gadt =
  | Int : int -> int gadt
  | Float : float -> float gadt

val gadt_of_int : int -> int gadt

val float_opt_of_gadt : 'a gadt -> float option
```
```OCaml
(* gadt_lib.ml *)
type _ gadt =
  | Int : int -> int gadt
  | Float : float -> float gadt

let gadt_of_int x = Int x

let float_opt_of_gadt : type a . a gadt -> float option = function
  | Float f -> Some f
  | _ -> None
```
```OCaml
(* gadt_bin.ml *)
let () =
  let open Gadt_lib in
  let x = 0 in
  let gadt = gadt_of_int x in
  let f = float_opt_of_gadt gadt in
  assert (f = None)
```

Before looking at the analysis results, let's look at the code.

The `Gadt_lib` defines one gadt `gadt` with 2 constructors : `Int` and `Float`.
The first one is used to construct values in `gadt_of_int`. The second one is
matched on in `float_opt_of_gadt`.
Following the classical variant constructor semantics, `Int` is used and `Float` is not.

Compile and analyze :
```
$ make -C gadt
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
ocamlopt -w +37 -bin-annot gadt_lib.mli gadt_lib.ml gadt_bin.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
/tmp/docs/fields_and_constructors/code_constructs/gadt/gadt_lib.mli:4: gadt.Float

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
```

As expected, the analyzer reports `gadt.Float` as unused.

## Removing the unused constructor

> [!TIP]
> Do not forget to remove `gadt.Float` from both the `.mli` **and** the `.ml`.
> Otherwise, the compiler will reject the code with a message like :
> ```
> File "gadt_lib.ml", line 1:
> Error: The implementation gadt_lib.ml
>        does not match the interface gadt_lib.mli:
>        Type declarations do not match:
>          type _ gadt = Int : int -> int gadt | Float : float -> float gadt
>        is not included in
>          type _ gadt = Int : int -> int gadt
>        An extra constructor, Float, is provided in the first declaration.
>        File "gadt_lib.mli", lines 2-3, characters 0-25: Expected declaration
>        File "gadt_lib.ml", lines 2-4, characters 0-31: Actual declaration
> ```

After removing the unused constructor, the compiler will report errors at the
locations were it was de-structured. Fixing those errors is simply done by
removing the invalid pattern branches.

Compile :
```
$ make -C gadt build
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
ocamlopt -w +37 -bin-annot gadt_lib.mli gadt_lib.ml gadt_bin.ml
File "gadt_lib.ml", line 8, characters 4-9:
8 |   | Float f -> Some f
        ^^^^^
Error: This variant pattern is expected to have type a gadt
       There is no constructor Float within type gadt
make: *** [Makefile:6: build] Error 2
make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
```
Fix and compile :
```
$ make -C gadt build
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
ocamlopt -w +37 -bin-annot gadt_lib.mli gadt_lib.ml gadt_bin.ml
make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
```

Compilation succeeds without error. Let's look at the code and analyze it.
```OCaml
(* gadt_lib.mli *)
type _ gadt =
  | Int : int -> int gadt

val gadt_of_int : int -> int gadt

val float_opt_of_gadt : 'a gadt -> float option
```
```OCaml
(* gadt_lib.ml *)
type _ gadt =
  | Int : int -> int gadt

let gadt_of_int x = Int x

let float_opt_of_gadt : type a . a gadt -> float option = function
  | _ -> None
```
```OCaml
(* gadt_bin.ml *)
let () =
  let open Gadt_lib in
  let x = 0 in
  let gadt = gadt_of_int x in
  let f = float_opt_of_gadt gadt in
  assert (f = None)
```

Analyze :
```
$ make -C gadt analyze
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/gadt'
```

Neither the compiler nor the analyzer reports any unused constructor.
Our work here is done.

> [!NOTE]
> The clean-up focused on the reports and the warnings. Of course, in the
> context, the `float_opt_of_gadt` became meaningless because it always
> produces `None`. Thus a user could decide to simply remove it and fix the
> call-sites by either replacing the function call by `None` or any better
> suited improvement.
