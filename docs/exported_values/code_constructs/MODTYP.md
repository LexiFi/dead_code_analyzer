The reference files for this example are in the
[modtyp](../../../examples/docs/exported_values/code_constructs/modtyp) directory.

The reference takes place in `/tmp/docs/exported_values/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/exported_values/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C modtyp build
```

The analysis command is :
```
make -C modtyp analyze
```

The compile + analyze command is :
```
make -C modtyp
```

> [!IMPORTANT]
> **LIMITATION**
>
> In order to reduce noise (false positives and duplication) in the results,
> the analyzer currently ignores values exported by module types
> (see [issue #50](https://github.com/LexiFi/dead_code_analyzer/issues/50)).

## First run

Code:
```OCaml
(* modtyp_lib.mli *)
module type T = sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end

module M_reuse : T

module M_constr : T with type t = unit

module M_subst : T with type t := unit

module M_redef : sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end
```
```OCaml
(* modtyp_lib.ml *)
module type T = sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end

module M = struct
  type t = unit
  let externally_used = ()
  let internally_used = ()
  let unused = ()
  let unused_unexported = ()
end

let () = M.internally_used

module M_reuse = M

module M_constr = M

module M_subst = M

module M_redef = M
```
```OCaml
(* modtyp_bin.ml *)
let () =
  ignore Modtyp_lib.M_reuse.externally_used;
  ignore Modtyp_lib.M_constr.externally_used;
  ignore Modtyp_lib.M_subst.externally_used;
  ignore Modtyp_lib.M_redef.externally_used
```

Before looking at the analysis results, let's look at the code.

The `Modtyp_lib` exports 1 module type `T` and 4 modules :
`M_reuse`, `M_constr`, `M_subst`, and `M_redef`. Of these 4 modules, the first
3 have `T` as signature (with minor twists), while the last one has its own
explicit signature, which is a copy of `T`. In this way, `M_redef` is equivalent
to `Module_lib.M` in the [module](MODULE.md) example : it exposes exactly
the same information.
Each of the modules exposed by `Modtyp_lib` are used exactly in the same way :
their `externally_used` values are explicitly referenced in `Modtyp_bin`.

One could expect that all the exported values are reported except for the
`externally_used`. However, reporting e.g. `M_subst.internally_used` as unused
would not be immediately actionable. In reality, this value is explicitly
declared by `T`.
Fixing an unused value reported in a module using a module type as signature
would require either removing the value from the module type (if possible),
or explicilty describing the signature of the module, effectively losing the
benefits of using the module type. Thus, reporting unused values for the module
itself would be counterproductive.

An actionable report would be of the value in the module type itself,
if it is unused by all the modules of that module type (as it is the case here
for `T.unused`). Currently, and as described in the introduction of this example,
the values exported by module types are ignored by the analyzer, and,
consequently, are not reported.

Now that we have explained what the expected behavior of the analyzer should be,
let's look at its results on the code above.

Compile and analyze :
```
$ make -C modtyp
make: Entering directory '/tmp/docs/exported_values/code_constructs/modtyp'
ocamlopt -w +32 -bin-annot modtyp_lib.mli modtyp_lib.ml modtyp_bin.ml
File "modtyp_lib.ml", line 14, characters 6-23:
14 |   let unused_unexported = ()
           ^^^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_unexported.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/modtyp/modtyp_lib.mli:18: M_redef.internally_used
/tmp/docs/exported_values/code_constructs/modtyp/modtyp_lib.mli:19: M_redef.unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/modtyp'
```

As in the module example, the compiler reports that `unused_unexported` is unused.

As in the module example, the analyzer reports `M_redef.internally_used` and
`M_redef.unused` as unused exported values.

All the reports are similar to those of the [module](MODULE.md) example.
Its exploration and resolution can be applied.
Our work here is done.
