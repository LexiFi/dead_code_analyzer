The reference files for this example are in the
[module](../../../examples/docs/exported_values/code_constructs/module) directory.

The reference takes place in `/tmp/docs/exported_values/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/exported_values/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C module build
```

The analysis command is :
```
make -C module analyze
```

The compile + analyze command is :
```
make -C module
```

## First run

Code:
```OCaml
(* module_lib.mli *)
module M : sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end
```
```OCaml
(* module_lib.ml *)
module M = struct
  type t = unit
  let externally_used = ()
  let internally_used = ()
  let unused = ()
  let unused_unexported = ()
end

let () = M.internally_used
```
```OCaml
(* module_bin.ml *)
let () =
  ignore Module_lib.M.externally_used
```

Before looking at the analysis results, let's look at the code.

All the values of `Module_lib.M` are exported except for
`unused_unexported`. Among the exported values, `unused` is not referenced
anywhere, `internally_used` is only referenced within its compilation unit
(`Module_lib`), and `externally_used` is only referenced outside of it.

> [!IMPORTANT]
> Using `internally_used` inside of `M` rather than outside would provide the
> same results. The only scope of interest is the compilation unit.

Compile and analyze :
```
$ make -C module
make: Entering directory '/tmp/docs/exported_values/code_constructs/module'
ocamlopt -w +32 -bin-annot module_lib.mli module_lib.ml module_bin.ml
File "module_lib.ml", line 7, characters 6-23:
7 |   let unused_unexported = ()
          ^^^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_unexported.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/module/module_lib.mli:5: M.internally_used
/tmp/docs/exported_values/code_constructs/module/module_lib.mli:6: M.unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/module'
```

The compiler reports that `unused_unexported` is unused.

The analyzer reports `M.internally_used` and `M.unused` as unused. Notice how
it did not only report the name of the value but its full path within its
compilation unit.

> [!NOTE]
> `M.internally_used` is only used within its compilation unit. Because it is
> declared in a `.mli`, only external uses are accounted for.
> It is left as an exercise to the reader to explore this example without
> `module_lib.mli`

## Removing the unused values

Code:
```OCaml
(* module_lib.mli *)
module M : sig
  type t
  val externally_used : t
end
```
```OCaml
(* module_lib.ml *)
module M = struct
  type t = unit
  let externally_used = ()
  let internally_used = ()
  let unused = ()
end

let () = M.internally_used
```
```OCaml
(* module_bin.ml *)
let () =
  ignore Module_lib.M.externally_used
```

Compile and analyze :
```
$ make -C module
make: Entering directory '/tmp/docs/exported_values/code_constructs/module'
ocamlopt -w +32 -bin-annot module_lib.mli module_lib.ml module_bin.ml
File "module_lib.ml", line 6, characters 6-12:
6 |   let unused = ()
          ^^^^^^
Warning 32 [unused-value-declaration]: unused value unused.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/module'
```

The compiler reports `unused` as unused and the analyzer does not report
anything. Removing that value fixes all the warnings. Our work here is done.
