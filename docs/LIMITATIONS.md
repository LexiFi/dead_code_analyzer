# Table of contents

+ [Module type](#module-type)
+ [Include module type with substitution](#include-module-type-with-substitution)

The limitations listed below apply to all the reportable elements of code.
Coding style issues are not concerned unless specified otherwise.

# Module type

Related issue :
[issue #50](https://github.com/LexiFi/dead_code_analyzer/issues/50).

As explained in the
[exported values' Module type](./exported_values/code_constructs/MODTYP.md)
example, the analyzer is currently restrcited to not reporting values declared
in module types. This is actually true for all reportable elements of code.
This means that any unused element defined by a module with a module type as
signature, even with constraints or substitutions, will not be reported.

A future improvement would be to report unused elements declared in module types
by considering all the elemnts defined in modules of such types as instances
of the elements in the module types.

# Include module type with substitution

Related issue :
[issue #64](https://github.com/LexiFi/dead_code_analyzer/issues/64).

According to the above limitation on elements in module types, elements included
from a module type should not be reported. Even more so according to the
semantics described in the
[exported values' Include](./exported_values/code_constructs/INCLUDE.md)
example.
This is the case unless there is a substitution on the module type.

## Example

The reference files for this example are in the
[sigincl](../../examples/docs/limitations/sigincl) directory.

The reference takes place in `/tmp/docs/limitations`, which
is a copy of the [limitations](../../examples/docs/limitations) directory.
Reported locations may differ depending on the location of the source files.

The compilation command is :
```
make -C sigincl build
```

The analysis command is :
```
make -C sigincl analyze
```

The compile + analyze command is :
```
make -C sigincl
```

Code:
```OCaml
(* sigincl_lib.mli *)
module type T = sig
  type t
  val x : t
end

module M : T

module I : sig
  include T with type t := unit
end
```
```OCaml
(* sigincl_lib.ml *)
module type T = sig
  type t
  val x : t
end

module M = struct
  type t = unit
  let x = ()
end

module I = M
```

Compile and analyze:
```
$ make -C sigincl
make: Entering directory '/tmp/docs/limitations/sigincl'
ocamlopt -bin-annot sigincl_lib.mli sigincl_lib.ml
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/limitations/sigincl/sigincl_lib.mli:10: I.x

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/limitations/sigincl'
```

The analyzer reports `I.x` at the line where `T` is included although it is
actually declared in `T`.
