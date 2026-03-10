The reference files for this example are in the
[functor](../../../examples/docs/exported_values/code_constructs/functor) directory.

The reference takes place in `/tmp/docs/exported_values/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/exported_values/code_constructs)
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
type t

module F (P : sig
  val used_required : t
  val unused_required : t
end) : sig
  val externally_used : t
  val internally_used : t
  val unused : t
end

module InternalParam : sig
  val used_required : t
  val unused_required : t
end

module ExternalParam : sig
  val used_required : t
  val unused_required : t
end

module InternalApp : sig
  val externally_used : t
  val internally_used : t
  val unused : t
end
```
```OCaml
(* functor_lib.ml *)
type t = unit

module F (P : sig
  val used_required : t
  val unused_required : t
end) = struct
  let externally_used = P.used_required
  let internally_used = P.used_required
  let unused = P.used_required
  let unused_unexported = P.used_required
  let () = internally_used
end

module InternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module ExternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module InternalApp = F(InternalParam)
```
```OCaml
(* functor_bin.ml *)
open Functor_lib

module ExternalApp = F(ExternalParam)

let () =
  ignore InternalApp.externally_used;
  ignore ExternalApp.externally_used
```

Before looking at the analysis results, let's look at the code.

The `Functor_lib` compilation unit exports 1 functor and 3 modules :
- `F` takes a module containing the values `used_required` and `unused_required`,
  and returns a module with 3 values whose names are explicit.
- `InternalParam` and `ExternalParam` fit the signature of `F`'s parameter `P`.
  The first one is used for a functor application inside `Functor_lib`.
  The second one is used for a functor application outside of it.
- `InternalApp` fits the signature of the result of `F`, and is impemented as
  the result of applying `F` inside its compilation unit.

The `Functor_bin` compilation unit exports 1 module : `ExternalApp`, which is
the result of applying `Functor_lib.F` outside its compilation unit.

Among all the exported values, the only explicit references accounted for are
`InternalApp.externally_used` and `External_app.externally_used` in
`Functor_bin`.

Additionally, some values are used by requirement. Because `InternalParam` and
`ExternalParam` are passed as arguments to `F`, their values `used_required` and
`unused_required` are used by requirement to fulfill the signature of `F`'s
parameter `P`.

With those observations in mind, let's see what the compiler and the analyzer
report.

Compile and analyze:
```
$ make -C functor
make: Entering directory '/tmp/docs/exported_values/code_constructs/functor'
ocamlopt -w +32 -bin-annot functor_lib.mli functor_lib.ml functor_bin.ml
File "functor_lib.ml", line 6, characters 2-25:
6 |   val unused_required : t
      ^^^^^^^^^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_required.

File "functor_lib.ml", line 11, characters 6-23:
11 |   let unused_unexported = P.used_required
           ^^^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_unexported.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:9: F.internally_used
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:10: F.unused
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:14: InternalParam.used_required
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:15: InternalParam.unused_required
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:25: InternalApp.internally_used
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:26: InternalApp.unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/functor'
```

> The compiler reports 2 unused values, that can be removed at the reported locations :
> - `unused_required`, defined by `P`, the parameter of `F`;
> - `unexported_unused`, defined by `F`, like it did in the [module](MODULE.md) example.

The analyzer reports 6 unused exported values, all in `functor_lib.mli` :
2 in `F`, 2 in `InternalParam`, and 2 in `InternalApp`. Let's observe them by
module (in reverse order) :

- The reports for `InternalApp` are identical to the [module](MODULE.md) example.
Although, `InternalApp` is implemented as the result of applying `F` in
`functor_lib.ml`, its signature is independent of `F` in `functor_lib.mli`.
I.e. it exposes its own values and the link between them and those of
`F` is absent from the signature. Consequently, they are tracked independently.

- As we observed before runnning the analyzer, the values in `InternalParam` are
used by requirement. However, this use is internal to `Functor_lib`, and there
is an interface available : `functor_lib.mli`. Consequently, the internal uses
are ignored, and `InternalParam`'s values become unused.

- `F` is a functor but is tracked like a regular module. The reported values are
those of its result module. Reporting on those may feel like duplicates, but,
as explained for the reports of `InternalApp`, the values of the result
module are declared independently of those of `InternalApp`, hence they are
tracked and reported independently.

All the values reported by the analyzer can be safely removed.

Before moving on, there is another observation that we can make :
the values `unused` and `internally_used` of `ExternalApp` are not reported.
Because they are reported for `InternalApp` and `F`, one could
expect them to be reported for `ExternalApp` as well. In reality, they are not
tracked individually for `ExternalApp` because it does not expose them
explicitly. Unlike `InternalApp` which has an explicit module signature,
`ExternalApp` does not. Consequently, its values are directly linked to those of
`F`. This situation is explored in the
[module signature](MODSIG.md) example.

> [!TIP]
> If we activated the compiler warning 67 `unused-functor-parameter` (by
> passing the argument `-w +67`), then the compiler would have reported :
> ```
> File "functor_lib.mli", line 4, characters 10-11:
> 4 | module F (P : sig
>               ^
> Warning 67 [unused-functor-parameter]: unused functor parameter P.
> ```
> This can be fixed by either replacing `P` with `_`, or by rewriting the
> declaration of `F` as :
> ```OCaml
> module F : sig
>     val used_required : t
>     val unused_required : t
>   end
>   -> sig
>     val externally_used : t
>     val internally_used : t
>     val unused : t
>   end
> ```

## Removing the unused values

In addition to removing everything that was reported by the compiler and the
analyzer, we also commented out `P.used_required` and `InternalParam` in
`functor_lib.mli`. It is up to the user to decide whether they would like to
keep them or remove them from their specifications. Neither would be reported
by the compiler or the analyzer.

> [!NOTE]
> It is allowed to give a larger module as argument than what the parameter
> specifies. Similarly it is allowed to declare the parameter larger in the
> interface than it is in the implementation. Consequently, the compiler would
> not complain if  `P` expected `unused_required` in the `.mli` but not in the
> `.ml`.

Code:
```OCaml
(* functor_lib.mli *)
type t

module F (P : sig
  val used_required : t
  (* val unused_required : t *)
end) : sig
  val externally_used : t
end

(*
module InternalParam : sig
end
*)

module ExternalParam : sig
  val used_required : t
  val unused_required : t
end

module InternalApp : sig
  val externally_used : t
end
```
```OCaml
(* functor_lib.ml *)
type t = unit

module F (P : sig
  val used_required : t
end) = struct
  let externally_used = P.used_required
  let internally_used = P.used_required
  let unused = P.used_required
  let () = internally_used
end

module InternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module ExternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module InternalApp = F(InternalParam)
```
```OCaml
(* functor_bin.ml *)
open Functor_lib

module ExternalApp = F(ExternalParam)

let () =
  ignore InternalApp.externally_used;
  ignore ExternalApp.externally_used
```

Compile and analyze :
```
$ make -C functor
make: Entering directory '/tmp/docs/exported_values/code_constructs/functor'
ocamlopt -w +32 -bin-annot functor_lib.mli functor_lib.ml functor_bin.ml
File "functor_lib.ml", line 9, characters 6-12:
9 |   let unused = P.used_required
          ^^^^^^
Warning 32 [unused-value-declaration]: unused value unused.

File "functor_lib.ml", line 15, characters 6-21:
15 |   let unused_required = ()
           ^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_required.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:18: ExternalParam.unused_required

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/functor'
```

Now that `F.unused` and `InternalParam.unused_required` are not exported, they
are reported as unused by the compiler, and can be removed safely.

`ExternalParam.unused_required` was used by requirement. Now that it is not
required by `P` (because it is commented out), it is unused and the analyzer
correctly reports it. It can be removed safely. Removing it will trigger the
same compiler warning as for `InternalParam.unused_required`, so it can be
removed from both the interface and the implementation.

The unused values can be removed as explained. Our work here is done.
