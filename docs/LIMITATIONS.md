# Table of contents

+ [Module type](#module-type)
+ [Module type with and inclusion](#module-type-with-and-inclusion)

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

# Module type with and inclusion

Related issue :
[issue #64](https://github.com/LexiFi/dead_code_analyzer/issues/64).

According to the above limitation on elements in module types, elements included
from a module type (`include T`) should not be reported. Even more so according
to the semantics described in the
[exported values' Include](./exported_values/code_constructs/INCLUDE.md)
example.
Similarly, elements defined in a module type with constraints (`T with ...`)
should not be reported.

This is mostly the case for unused exported values, unless there is a substition
(`T with ... := ...`) and not .mli is provided, or a .mli is provided and the 2
contructs are mixed into an included module type with substitution
(`include T with ... := ...`).
This is however, not the case at all for constructors, fields, and methods.
Optional arguments always/never used are only affected by substitutions.

## Example

The reference files for this example are in the
[modtype](../../examples/docs/limitations/modtype) directory.

The reference takes place in `/tmp/docs/limitations`, which
is a copy of the [limitations](../../examples/docs/limitations) directory.
Reported locations may differ depending on the location of the source files.

The compilation command is :
```
make -C modtype build
```

The analysis command is :
```
make -C modtype analyze
```

The compile + analyze command is :
```
make -C modtype
```

Code:
```OCaml
(* modtype_with_intf.mli *)
module type T = sig
  type t
  type ctor = Ctor
  type field = {field : unit}
  val x : t
  val o : < m : t >
  class c : object method m : t end
  val f : ?always:t -> ?never:t -> unit -> t
  (* to use as [always] in call to [f] *)
  val always : t
end

module Regular : T

module With : T with type t = int

module Subst : T with type t := int

module Incl : sig
  include T
end

module Incl_with : sig
  include T with type t = int
end

module Incl_subst : sig
  include T with type t := int
end

module Ftor () : T

module Ftor_with () : T with type t = int

module Ftor_subst () : T with type t := int
```
The other files are not displayed here.
`modtype_without_intf` is equivalent to `modtype_with_intf`'s interface and
implementation merged together. `modtype_bin` uses the function's `f` to track
its optional parameters.
The modules are all defined as:
```OCaml
struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end
```

Compile and analyze:
```
$ make -C modtype
make: Entering directory '/tmp/docs/limitations/modtype'
ocamlopt -bin-annot modtype_with_intf.mli modtype_with_intf.ml modtype_without_intf.ml modtype_bin.ml
dead_code_analyzer --all -E threshold:4 -M threshold:4 -T threshold:4 .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================


.>->  ALMOST UNUSED EXPORTED VALUES: Called 2 time(s):
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.always
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.f
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.o
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.x
--------


.>->  ALMOST UNUSED EXPORTED VALUES: Called 4 time(s):
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.always
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.f
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.o
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.x
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.always
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.f
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.o
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.x
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.always
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.f
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.o
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.x

Nothing else to report in this section
--------------------------------------------------------------------------------


.> UNUSED METHODS:
=================
/tmp/docs/limitations/modtype/modtype_with_intf.mli:7: Ftor_with.o#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:7: Incl.o#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:7: Incl_with.o#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:7: With.o#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:8: Ftor_with.c#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:8: Incl.c#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:8: Incl_with.c#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:8: With.c#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:18: Subst.c#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:18: Subst.o#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.c#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.o#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:36: Ftor_subst.c#m
/tmp/docs/limitations/modtype/modtype_with_intf.mli:36: Ftor_subst.o#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:7: Ftor_with.o#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:7: Incl.o#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:7: Incl_with.o#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:7: With.o#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:8: Ftor_with.c#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:8: Incl.c#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:8: Incl_with.c#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:8: With.c#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.c#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.o#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.c#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.o#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.c#m
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.o#m
--------



Nothing else to report in this section
--------------------------------------------------------------------------------


.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
/tmp/docs/limitations/modtype/modtype_with_intf.mli:4: Ftor_with.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_with_intf.mli:4: Incl.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_with_intf.mli:4: Incl_with.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_with_intf.mli:4: With.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_with_intf.mli:5: Ftor_with.field.field
/tmp/docs/limitations/modtype/modtype_with_intf.mli:5: Incl.field.field
/tmp/docs/limitations/modtype/modtype_with_intf.mli:5: Incl_with.field.field
/tmp/docs/limitations/modtype/modtype_with_intf.mli:5: With.field.field
--------


.>->  ALMOST UNUSED CONSTRUCTORS/RECORD FIELDS: Called 2 time(s):
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/tmp/docs/limitations/modtype/modtype_with_intf.mli:18: Subst.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_with_intf.mli:18: Subst.field.field
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: Incl_subst.field.field
/tmp/docs/limitations/modtype/modtype_with_intf.mli:36: Ftor_subst.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_with_intf.mli:36: Ftor_subst.field.field
--------


.>->  ALMOST UNUSED CONSTRUCTORS/RECORD FIELDS: Called 4 time(s):
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/tmp/docs/limitations/modtype/modtype_without_intf.ml:4: Ftor_with.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_without_intf.ml:4: Incl.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_without_intf.ml:4: Incl_with.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_without_intf.ml:4: With.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_without_intf.ml:5: Ftor_with.field.field
/tmp/docs/limitations/modtype/modtype_without_intf.ml:5: Incl.field.field
/tmp/docs/limitations/modtype/modtype_without_intf.ml:5: Incl_with.field.field
/tmp/docs/limitations/modtype/modtype_without_intf.ml:5: With.field.field
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: Subst.field.field
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: Incl_subst.field.field
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.ctor.Ctor
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: Ftor_subst.field.field

Nothing else to report in this section
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/limitations/modtype/modtype_with_intf.mli:9: ?always
/tmp/docs/limitations/modtype/modtype_with_intf.mli:18: ?always
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: ?always
/tmp/docs/limitations/modtype/modtype_with_intf.mli:36: ?always
/tmp/docs/limitations/modtype/modtype_without_intf.ml:9: ?always
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: ?always
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: ?always
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: ?always

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================
/tmp/docs/limitations/modtype/modtype_with_intf.mli:9: ?never
/tmp/docs/limitations/modtype/modtype_with_intf.mli:18: ?never
/tmp/docs/limitations/modtype/modtype_with_intf.mli:29: ?never
/tmp/docs/limitations/modtype/modtype_with_intf.mli:36: ?never
/tmp/docs/limitations/modtype/modtype_without_intf.ml:9: ?never
/tmp/docs/limitations/modtype/modtype_without_intf.ml:36: ?never
/tmp/docs/limitations/modtype/modtype_without_intf.ml:74: ?never
/tmp/docs/limitations/modtype/modtype_without_intf.ml:108: ?never

Nothing else to report in this section
--------------------------------------------------------------------------------


.> CODING STYLE:
===============

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/limitations/modtype'
```
> [!NOTE]
> The analyzer is run with thresholds. Because elements of `T` share the same
> location at the point of use of `T`, the analyzer gets confused and considers
> some unused elements as used. Using thresholds enables reporting elements used
> up to a fixed amount.
> For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

The analyzer reports elements although they are all defined within module type
`T`.
Following its semantics on module types, the report should be empty.
