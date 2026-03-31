The reference files for this example are in the
[inline\_record](../../../examples/docs/fields_and_constructors/code_constructs/inline_record) directory.

The reference takes place in `/tmp/docs/fields_and_constructors/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/fields_and_constructors/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C inline_record build
```

The analysis command is :
```
make -C inline_record analyze
```

The compile + analyze command is :
```
make -C inline_record
```

> [!IMPORTANT]
> **LIMITATION**
>
> The analyzer ignores fields in inline records.

## First run

Code :
```OCaml
(* inline_record_lib.mli *)
type ('a, 'b) t =
  | Both of {left : 'a; right : 'b}
  | Left of 'a
  | Right of 'b

val get_left_opt : ('a, 'b) t -> 'a option
```
```OCaml
(* inline_record_lib.ml *)
type ('a, 'b) t =
  | Both of {left : 'a; right : 'b}
  | Left of 'a
  | Right of 'b

let get_left_opt = function
  | Both {left; _}
  | Left left -> Some left
  | _ -> None
```
```OCaml
(* inline_record_bin.ml *)
let () =
  let open Inline_record_lib in
  let both = Both {left = 1; right = "one"} in
  match get_left_opt both with
  | Some left -> assert (left = 1)
  | _ -> assert false
```

Before looking at the analysis results, let's look at the code.

The `Inline_record_lib` defines 1 variant type `t` with 3 constructors : `Both`,
`Left`, and `Right`. Only the first one is used, the 2 others are only matched
on in `get_left_opt`. Constructor `Both` has an inline record argument with 2
fields : `left`, and `right`. Only the first one is used, the other one is only
written to.
Following the report semantics on constructors and fields, `Left`, `Right`, and
`right` should be reported unused.

However, the analyzer does not report unused inline record fields.
The compiler also does not warn on unused inline record fields.

Compile and analyze :
```
$ make -C inline_record
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/inline_record'
ocamlopt -w +37+69 -bin-annot inline_record_lib.mli inline_record_lib.ml inline_record_bin.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
/tmp/docs/fields_and_constructors/code_constructs/inline_record/inline_record_lib.mli:4: t.Left
/tmp/docs/fields_and_constructors/code_constructs/inline_record/inline_record_lib.mli:5: t.Right

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/inline_record'
```

As expected, `t.Left`, and `t.Right` are reported unused by the analyzer.
As explained, the analyzer does not report inline record fields so `right` is
not reported.
Fixing the unused constructors is the same as in the
[Polymorphic type](./POLYMORPHIC_TYPE.md) example. Our work here is done.
