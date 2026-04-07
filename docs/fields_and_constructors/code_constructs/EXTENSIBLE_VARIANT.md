The reference files for this example are in the
[extensible\_variant](../../../examples/docs/fields_and_constructors/code_constructs/extensible_variant) directory.

The reference takes place in `/tmp/docs/fields_and_constructors/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/fields_and_constructors/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C extensible_variant build
```

The analysis command is :
```
make -C extensible_variant analyze
```

The compile + analyze command is :
```
make -C extensible_variant
```

> [!IMPORTANT]
> **LIMITATION**
>
> The analyzer ignores extensible variant constructors.

## First run

Code :
```OCaml
(* extensible_variant_lib.mli *)
type t = ..
type t +=
  | Int of int
  | Float of float

val to_string_opt : t -> string option
```
```OCaml
(* extensible_variant_lib.ml *)
type t = ..
type t +=
  | Int of int
  | Float of float

let to_string_opt = function
  | Int i -> Some (string_of_int i)
  | Float f -> Some (string_of_float f)
  | _ -> None
```
```OCaml
(* extensible_variant_bin.ml *)
open Extensible_variant_lib
type t += String of string

let to_string_opt = function
  | String s -> Some s
  | t -> to_string_opt t

let () =
  match to_string_opt (Int 42) with
  | Some s -> print_endline s
  | None -> ()

```

Before looking at the analysis results, let's look at the code.

The `Extensible_variant_lib` deines 1 extensbile variant type `t` and adds 2
constructors to it : `Int` and `Float`. `Extensible_variant_bin` adds another
constructor to `t` : `String`. Of the 3 constructors, only `Int` is used to
build a value. All of the constructors are matched on.
Following the classical variant constructor semantics, `Int` is used while
`Float` and `String are not.

One could expect the analyzer to report `Float` and `String` as unused.
However, the analyzer does not report unused extensible variant constructors.

The compiler does report unused extensible variant constructors via warning 38.

Compile and analyze :
```
$ make -C extensible_variant
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/extensible_variant'
ocamlopt -w +38 -bin-annot extensible_variant_lib.mli extensible_variant_lib.ml extensible_variant_bin.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/extensible_variant'
```

As explained, nothing is reported. Our work here is done.
