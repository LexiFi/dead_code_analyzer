The reference files for this example are in the
[include](../../../examples/docs/exported_values/code_constructs/include) directory.

The reference takes place in `/tmp/docs/exported_values/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/exported_values/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C include build
```

The analysis command is :
```
make -C include analyze
```

The compile + analyze command is :
```
make -C include
```

## First run

Code:
```OCaml
(* include_lib.ml *)
module Original = struct
  let used_directly = ()
  let used_indirectly = ()
  let unused = ()
end

module Reexport = struct
  include Original
end

module Redefine = struct
  include Original
  let used_directly = ()
  let unused = ()
end
```
```OCaml
(* include_bin.ml *)
let () =
  let open Include_lib in
  ignore Original.used_directly;
  ignore Reexport.used_indirectly;
  ignore Redefine.used_directly;
```

Before looking at the analysis results, let's look at the code.

The `Include_lib` compilation unit does not have a `.mli`, so all the internal
uses are accounted for. It exposes 3 modules :
- `Original`, which explicitly defines all its values;
- `Reexport`, which only includes `Original`;
- `Redefine`, which includes `Original` and redefines 2 values :
  `used_directly`, and `unused`.

By the explanation in the [module signature](MODSIG.md) and
[module type](MODTYP.md) examples, although there are 9 exported values
(`used_directly`, `used_indirectly`, and `unused` for each module), only 5 are
expected to be tracked by the analyzer : those in `Original` and the 2 redefined
in `Redefine`. These are the only values a developer can trivially remove if
they are reported unused.

Thus, the only values used are `Original.used_directly`,
`Original.used_indirectly` (by an explicit reference to
`Reexport.used_indirectly`), and `Redefine.used_directly`. This means that the
unused exported values tracked by the analyzer are `Original.unused` and
`Redefine.unused`.

Compile and analyze:
```
$ make -C include
make: Entering directory '/tmp/docs/exported_values/code_constructs/include'
ocamlopt -bin-annot include_lib.ml include_bin.ml
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/include/include_lib.ml:5: Original.unused
/tmp/docs/exported_values/code_constructs/include/include_lib.ml:15: Redefine.unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/include'
```

The compiler does not report any unused value.

As expected, the analyzer reports `Original.unused` and `Redefine.unused`.

## Removing the unused values

The reported values can be removed from the implementation.

Code:
```OCaml
(* include_lib.ml *)
module Original = struct
  let used_directly = ()
  let used_indirectly = ()
end

module Reexport = struct
  include Original
end

module Redefine = struct
  include Original
  let used_directly = ()
end
```
```OCaml
(* include_bin.ml *)
let () =
  let open Include_lib in
  ignore Original.used_directly;
  ignore Reexport.used_indirectly;
  ignore Redefine.used_directly;
```

Compile and analyze:
```
$ make -C include
make: Entering directory '/tmp/docs/exported_values/code_constructs/include'
ocamlopt -bin-annot include_lib.ml include_bin.ml
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/include'
```

Now, neither the compiler nor the analyzer report any unused value.
Our work here is done.
