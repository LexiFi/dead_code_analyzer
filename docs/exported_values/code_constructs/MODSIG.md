The reference files for this example are in the
[modsig](../../../examples/docs/exported_values/code_constructs/modsig) directory.

The reference takes place in `/tmp/docs/exported_values/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/exported_values/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C modsig build
```

The analysis command is :
```
make -C modsig analyze
```

The compile + analyze command is :
```
make -C modsig
```

## First run

Code:
```OCaml
(* modsig_lib.ml *)
module Original = struct
  let used_directly = ()
  let used_indirectly = ()
  let used_by_requirement = ()
  let unused = ()
end

module Alias_without_sig = Original

module Alias_with_sig : sig
  val used_by_requirement : unit
end = Original
```
```OCaml
(* modsig_bin.ml *)
let () =
  let open Modsig_lib in
  Original.used_directly;
  Alias_without_sig.used_indirectly
```

Before looking at the analysis results, let's look at the code.

The `Modsig_lib` compilation unit does not have a `.mli`, so all the internal
uses are accounted for. It exposes 3 modules : `Original`, `Alias_without_sig`,
and `Alias_with_sig`. Only the first module actually defines values. The second
one is an trivial alias, and the third is an alias with an explicit signature.
Because the 2 latter modules are aliases of the `Original` module, one could
expect that the values they expose are unified with the ones of `Original`.
This would imply that only the values in `Original` could be reported as unused.
This reasoning is partially true.

As explained in the [module type](MODTYP.md) example, reporting a value in
`Alias_without_sig` would not be trivially solved. Thus,
its values are unified with those of `Original`, and, consequently, they
cannot be reported by the analyzer. Only the values in `Original` can be
reported in this case.

However, `Alias_with_sig` has an explicit signature, which means 2 things:
1. it controls what it exports, thus a reporting values in that module is
   trivially actionable by removing the reported values from the signature
2. it has requirements, thus all the values in `Original` that are expected in
   the signature of `Alias_with_sig` are considered used.

Now that we have explained the nuances introduced by the existence of an
explicit module signature, let's look at the results.

Compile and analyze:
```
$ make -C modsig
make: Entering directory '/tmp/docs/exported_values/code_constructs/modsig'
ocamlopt -bin-annot modsig_lib.ml modsig_bin.ml
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/modsig/modsig_lib.ml:6: Original.unused
/tmp/docs/exported_values/code_constructs/modsig/modsig_lib.ml:12: Alias_with_sig.used_by_requirement

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/modsig'
```

The compiler does not report any unused value.

The analyzer reports that `Original.unused` and
`Alias_with_sig.used_by_requirement` are unused. As expected, it does not report
any unused value in `Alias_without_sig`.
Let's look more closely at the values and their uses.
- `Original.used_directly` is explicitly referenced in `Modsig_bin`
- `Original.used_indirectly` is used by an explicit reference to
  `Alias_without_sig.used_indirectly` in `Modsig_bin`
- `Original.used_by_requirement` is used by requirement to fulfill
  `Alias_with_sig`'s signature
- `Original.unused` is not referenced nor required anywhere
- `Alias_without_sig` does not "own" any value
- `Alias_with_sig.used_by_requirement` is not referenced nor required anywhere

## Removing the unused values

The reported values can be removed : `Original.unused` is removed from the
module's strucutre because it does not have an explicit signature, and
`Alias_with_sig.used_by_requirement` is removed from the module's signature.

Code:
```OCaml
(* modsig_lib.ml *)
module Original = struct
  let used_directly = ()
  let used_indirectly = ()
  let used_by_requirement = ()
end

module Alias_without_sig = Original

module Alias_with_sig : sig end = Original
```
```OCaml
(* modsig_bin.ml *)
let () =
  let open Modsig_lib in
  Original.used_directly;
  Alias_without_sig.used_indirectly
```

Compile and analyze:
```
$ make -C modsig
make: Entering directory '/tmp/docs/exported_values/code_constructs/modsig'
ocamlopt -bin-annot modsig_lib.ml modsig_bin.ml
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/modsig/modsig_lib.ml:5: Original.used_by_requirement

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/modsig'
```

The compiler does not report any unused value.

The analyzer reports `Original.used_by_requirement` as unused. Indeed,
by removing `used_by_requirement` from the signature of `Alias_with_sig` we
removed the requirement for `Original` to provide it. This value can be removed
from `Orignal`, and neither the compiler nor the analyzer will report unused
values anymore. Our work here is done.
