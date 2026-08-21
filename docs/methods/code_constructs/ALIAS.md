The reference files for this example are in the
[alias](../../../examples/docs/methods/code_constructs/alias) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C alias build
```

The analysis command is :
```
make -C alias analyze
```

The compile + analyze command is :
```
make -C alias
```

## First run

Code:
```OCaml
(* alias_lib.mli *)
val original :
  < used : unit
  ; used_by_alias : unit
  ; unused : unit
  >

val alias :
  < used : unit
  ; used_by_alias : unit
  ; unused : unit
  >
```
```OCaml
(* alias_lib.ml *)
let original =
  object
    method used = ()
    method used_by_alias = ()
    method unused = ()
  end

let alias = original
```
```OCaml
(* alias_bin.ml *)
open Alias_lib

let () =
  original#used;
  alias#used_by_alias
```

Before looking at the analysis results, let's look at the code.

The `Alias_lib` declares and exports 2 objects : `original`, and `alias`.
They both have the same type, and `alias` is actually defined as equal to
`original`. Thus, `alias` is an alias for `original`.
Their methods are used in `Alias_bin`. The method `used` is used via
`original`, and `used_by_alias` is via `alias`.
Because `alias` is the same object as `original`, the 2 methods are actually
used for both objects.

Compile and analyze:
```
$ make -C alias
make: Entering directory '/tmp/docs/methods/code_constructs/alias'
ocamlopt -bin-annot alias_lib.mli alias_lib.ml alias_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/alias/alias_lib.mli:2: original#unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/alias'
```

As expected, only the method `unused` is reported. However, it is only
reported for `original`.
Due to the dynamic nature of objects, the analyzer reports the methods of the
objects that actually define them. Thus, because `alias` is an alias for
`original`, it does not define any new method, and the analyzer would not
report anything for it.

## Removing the unused methods

If we only remove `original#unused` from `alias_lib.ml` and `alias_lib.mli`,
we get the following error:
```
Error: The implementation alias_lib.ml
       does not match the interface alias_lib.mli:
       Values do not match:
         val alias : < used : unit; used_by_alias : unit >
       is not included in
         val alias : < unused : unit; used : unit; used_by_alias : unit >
       The type < used : unit; used_by_alias : unit >
       is not compatible with the type
         < unused : unit; used : unit; used_by_alias : unit >
       Type <  > is not compatible with type < unused : unit >
       File "alias_lib.mli", lines 7-11, characters 0-3: Expected declaration
       File "alias_lib.ml", line 8, characters 4-9: Actual declaration
```

This is because the .mli still exports `alias#unused`, although it is not
defined. The easy fix is to also remove `alias#unused` from the .mli.

> [!NOTE]
> Alternatively, if we wanted to keep `alias#unused`, we could swap the
> definitions, making `original` the alias (with a coercion to discard
> `unused` : `let original = (alias :> <used: unit; used_by_alias: unit>)`).
> The analyzer would then report `alias#unused` as unused.
>
> We could also define `alias` as a new object with methods `used` and
> `used_by_alias` equal to those of `original`. Then, `alias` would not
> longer be an alias for `original` and their methods would be tracked
> and reported independently. Thus, the analyzer would report `alias#unused`
> , and `alias#used`. It would not report any method for `original` because
> they are all used by `alias`.

Code:
```OCaml
(* alias_lib.mli *)
val original :
  < used : unit
  ; used_by_alias : unit
  >

val alias :
  < used : unit
  ; used_by_alias : unit
  >
```
```OCaml
(* alias_lib.ml *)
let original =
  object
    method used = ()
    method used_by_alias = ()
  end

let alias = original
```
```OCaml
(* alias_bin.ml *)
open Alias_lib

let () =
  original#used;
  alias#used_by_alias
```

Compile and analyze:
```
$ make -C alias
make: Entering directory '/tmp/docs/methods/code_constructs/alias'
ocamlopt -bin-annot alias_lib.mli alias_lib.ml alias_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/alias'
```

There is no more unused method. Our work here is done.
