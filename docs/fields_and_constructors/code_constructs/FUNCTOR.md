The reference files for this example are in the
[functor](../../../examples/docs/fields_and_constructors/code_constructs/functor) directory.

The reference takes place in `/tmp/docs/fields_and_constructors/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/fields_and_constructors/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C functor build
```

The analysis command is :
```
make -C functor analyze
```

The compile + analyze command is :
```
make -C functor
```

## First run

Code:
```OCaml
(* functor_lib.mli *)
module M : sig
  type record_externally_used_by_param_requirement = { f : int }
  type record_internally_used_by_param_requirement = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used_by_param_requirement = C
  type constructor_internally_used_by_param_requirement = C
  type constructor_unused = C
end

module External_functor (_ : sig
  type record_externally_used_by_param_requirement = { f : int }
  type constructor_externally_used_by_param_requirement = C
end) : sig
  type record_externally_used = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used = C
  type constructor_unused = C
end
```
```OCaml
(* functor_lib.ml *)
module M = struct
  type record_externally_used_by_param_requirement = { f : int }
  type record_internally_used_by_param_requirement = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used_by_param_requirement = C
  type constructor_internally_used_by_param_requirement = C
  type constructor_unused = C
end

module Internal_functor (_ : sig
  type record_internally_used_by_param_requirement = { f : int }
  type constructor_internally_used_by_param_requirement = C
end) = struct
end

module External_functor (_ : sig
  type record_externally_used_by_param_requirement = { f : int }
  type constructor_externally_used_by_param_requirement = C
end) = struct
  type record_externally_used = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used = C
  type constructor_unused = C
end

module _ = Internal_functor (M)
```
```OCaml
(* functor_bin.ml *)
open Functor_lib

module External_app = External_functor (M)

let () =
  let _ : External_app.constructor_externally_used = External_app.C in
  let _ = fun (r : External_app.record_externally_used) -> ignore r.f in
  ()
```

Before looking at the analysis results, let's look at the code.

The `Functor_lib` compilation unit exports 1 module and 1 functor :
- `M` is our primary module, which will be used as parameter for functors.
    It exports 6 types: 3 records with a single field `f`, and 3 regular
    variant types with a single constructor `C`.
    The type names are explicit.
- `External_functor` takes a module containing the types
    `record_externally_used_by_param_requirement` and
    `constructor_externally_used_by_param_requirement`,
    and returns a module with 4 types whose names are explicit.

In addition to the exported modules, `Functor_lib` also defines
`Internal_functor` which takes a module containing the types
`record_internally_used_by_param_requirement` and
`constructor_externally_used_by_param_requirement`, and returns an empty module.
It is applied with `M` as argument. Because `M` must fulfill the signature of
`Internal_functor`'s parameter, the expected types' constructors and fields,
are used by requirement.

In `Functor_bin`, `External_functor` is applied with `M` as argument. Thus,
once again, because the signature of the functor's parameter must be fulfilled,
the expected types' constructors and fields, are used by requirement.
The application's result is bound to `External_app`, and the constructors
and fields of its `*_externally_used` types are explicitly used.

This leaves the `*_unused` types' constructors and fields unused.

With those observations in mind, let's see what the compiler and the analyzer
report.

Compile and analyze:
```
$ make -C functor
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/functor'
ocamlopt -w +37+69 -bin-annot functor_lib.mli functor_lib.ml functor_bin.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
/tmp/docs/fields_and_constructors/code_constructs/functor/functor_lib.mli:5: M.record_unused.f
/tmp/docs/fields_and_constructors/code_constructs/functor/functor_lib.mli:9: M.constructor_unused.C
/tmp/docs/fields_and_constructors/code_constructs/functor/functor_lib.mli:17: External_functor.record_unused.f
/tmp/docs/fields_and_constructors/code_constructs/functor/functor_lib.mli:20: External_functor.constructor_unused.C

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/functor'
```

The analyzer reports 4 unused constructors and fields. They are the expected ones.

The constructors and fields of `External_app` are not reported. This is
because the module is not exported with its own signature but as the result
of the functor application `External_functor(M)`.

> [!NOTE]
> If `External_functor` was exported with its own signature, then its
> constructors and fields would be reported.
> Note that any use of an `External_app` constructor or field is considered
> as a use of the corresponding `External_functor`'s and vice-versa. See the
> documentation on [Equal types](./EQUAL_TYPES.md) for more information.


## Removing the unused constructors and fields

Because our types are composed of a single constructor or field, we can
remove them entirely.

If we only removed the types from `functor_lib.mli`, then the compiler would
warn us about the unused constructors and fields in `functor_lib.ml`.

Compile:
```
$ make -C functor build
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/functor'
ocamlopt -w +37+69 -bin-annot functor_lib.mli functor_lib.ml functor_bin.ml
File "functor_lib.ml", line 5, characters 25-32:
5 |   type record_unused = { f : int }
                             ^^^^^^^
Warning 69 [unused-field]: unused record field f.
File "functor_lib.ml", line 9, characters 28-29:
9 |   type constructor_unused = C
                                ^
Warning 37 [unused-constructor]: unused constructor C.
File "functor_lib.ml", line 23, characters 25-32:
23 |   type record_unused = { f : int }
                              ^^^^^^^
Warning 69 [unused-field]: unused record field f.
File "functor_lib.ml", line 26, characters 28-29:
26 |   type constructor_unused = C
                                 ^
Warning 37 [unused-constructor]: unused constructor C.
make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/functor'
```

Thus, we can update `functor_lib.ml` as well

Code:
```OCaml
(* functor_lib.mli *)
module M : sig
  type record_externally_used_by_param_requirement = { f : int }
  type record_internally_used_by_param_requirement = { f : int }

  type constructor_externally_used_by_param_requirement = C
  type constructor_internally_used_by_param_requirement = C
end

module External_functor (_ : sig
  type record_externally_used_by_param_requirement = { f : int }
  type constructor_externally_used_by_param_requirement = C
end) : sig
  type record_externally_used = { f : int }

  type constructor_externally_used = C
end
```
```OCaml
(* functor_lib.ml *)
module M = struct
  type record_externally_used_by_param_requirement = { f : int }
  type record_internally_used_by_param_requirement = { f : int }

  type constructor_externally_used_by_param_requirement = C
  type constructor_internally_used_by_param_requirement = C
end

module Internal_functor (_ : sig
  type record_internally_used_by_param_requirement = { f : int }
  type constructor_internally_used_by_param_requirement = C
end) = struct
end

module External_functor (_ : sig
  type record_externally_used_by_param_requirement = { f : int }
  type constructor_externally_used_by_param_requirement = C
end) = struct
  type record_externally_used = { f : int }

  type constructor_externally_used = C
end

module _ = Internal_functor (M)
```
```OCaml
(* functor_bin.ml *)
open Functor_lib

module External_app = External_functor (M)

let () =
  let _ : External_app.constructor_externally_used = External_app.C in
  let _ = fun (r : External_app.record_externally_used) -> ignore r.f in
  ()
```

Complie and analyze:
```
$ make -C functor
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/functor'
ocamlopt -w +37+69 -bin-annot functor_lib.mli functor_lib.ml functor_bin.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/functor'
```

Nothing is reported anymore. Our work here is done.

> [!TIP]
> If we activated the compiler warning 34 `unused-type-declaration`
> (by passing the argument `-w +34`), then the compiler would have reported
> the functors' parameters' types as unused:
> ```
> make -C functor build
> File "functor_lib.ml", line 11, characters 2-64:
> 11 |   type record_internally_used_by_param_requirement = { f : int }
>        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
> Warning 34 [unused-type-declaration]: unused type record_internally_used_by_param_requirement.
> File "functor_lib.ml", line 12, characters 2-59:
> 12 |   type constructor_internally_used_by_param_requirement = C
>        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
> Warning 34 [unused-type-declaration]: unused type constructor_internally_used_by_param_requirement.
> File "functor_lib.ml", line 17, characters 2-64:
> 17 |   type record_externally_used_by_param_requirement = { f : int }
>        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
> Warning 34 [unused-type-declaration]: unused type record_externally_used_by_param_requirement.
> File "functor_lib.ml", line 18, characters 2-59:
> 18 |   type constructor_externally_used_by_param_requirement = C
>        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
> Warning 34 [unused-type-declaration]: unused type constructor_externally_used_by_param_requirement.
> ```
