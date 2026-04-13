The reference files for this example are in the
[intext\_app](../../../examples/docs/optional_arguments/code_constructs/intext_app) directory.

The reference takes place in `/tmp/docs/optional_arguments/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/optional_arguments/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C intext_app build
```

The analysis command is :
```
make -C intext_app analyze
```

The compile + analyze command is :
```
make -C intext_app
```

## First run

Code:
```OCaml
(* intext_app_lib.mli *)
val max : ?min:'a -> 'a -> 'a -> 'a
val min : ?max:'a -> 'a -> 'a -> 'a
```
```OCaml
(* intext_app_lib.ml *)
let max ?min x y =
  let max x y = if x > y then x else y in
  let res = max x y in
  match min with
  | None -> res
  | Some m -> max m res

let min ?max x y =
  let min x y = if x < y then x else y in
  let res = min x y in
  match max with
  | None -> res
  | Some m -> min m res

let () =
  let max = max 0 42 in
  let min = min ~max 100 200 in
  assert (min = max)
```
```OCaml
(* intext_app_bin.ml *)
let () =
  let open Intext_app_lib in
  let min = min 0 42 in
  let max = max ~min 100 200 in
  assert (min = max)
```

Before looking at the analysis results, let's look at the code.

There are 2 functions defined and exported by `Intext_app_lib`: `max` and
`min`. They both have an optional argument, and both functions are used
internally and externally.
Optional argument `?min` of `max` is never used internally and always used
externally. Optional argument `?max` of `min` is always used internally and
never used externally.

Compile and analyze:
```
$ make -C intext_app
make: Entering directory '/tmp/docs/opt_args/code_constructs/intext_app'
ocamlopt -bin-annot intext_app_lib.mli intext_app_lib.ml intext_app_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/opt_args/code_constructs/intext_app/intext_app_lib.ml:9: ?max
/tmp/docs/opt_args/code_constructs/intext_app/intext_app_lib.mli:2: ?min

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================
/tmp/docs/opt_args/code_constructs/intext_app/intext_app_lib.ml:2: ?min
/tmp/docs/opt_args/code_constructs/intext_app/intext_app_lib.mli:3: ?max

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/opt_args/code_constructs/intext_app'
```

The analyzer reports `?max` and `?min` both as always used and as never used.
The difference in the reports are locations. The optional argument `?max` of the
internal `min` (in the `.ml`) is always used but its exported version (in the
`.mli`) is never used. Similarly, the optional argument `?min` of the internal
`max` (in the `.ml`) is never used but its exported version (in the `.mli`) is
always used.

> [!IMPORTANT]
> If a module has an explicit signature, the analyzer tracks internal and external
> uses of its optional arguments separately, and, thus, reports separately their
> structure and signature definitions.
> For modules without explicit signature, the structure and signature definitions
> are the same so the analyzer does not distinguish internal and external uses.

## Fixing the reports on `?max`

`?max` is always used internally and never used externally. We can simplify the
signature of the exported function and split the `min` function in 2 :
- `bounded_min` which expects a mandatory labelled argument `~max`;
- `min` which only expects 2 arguments.

Code:
```OCaml
(* intext_app_lib.mli *)
val max : ?min:'a -> 'a -> 'a -> 'a
val min : 'a -> 'a -> 'a
```
```OCaml
(* intext_app_lib.ml *)
let max ?min x y =
  let max x y = if x > y then x else y in
  let res = max x y in
  match min with
  | None -> res
  | Some m -> max m res

let min x y  = if x < y then x else y

let bounded_min ~max x y =
  let res = min x y in
  min max res

let () =
  let max = max 0 42 in
  let min = bounded_min ~max 100 200 in
  assert (min = max)
```
```OCaml
(* intext_app_bin.ml *)
let () =
  let open Intext_app_lib in
  let min = min 0 42 in
  let max = max ~min 100 200 in
  assert (min = max)
```

`bounded_min` is only used internally so we do not export it.

Compile and analyze:
```
$ make -C intext_app
make: Entering directory '/tmp/docs/opt_args/code_constructs/intext_app'
ocamlopt -bin-annot intext_app_lib.mli intext_app_lib.ml intext_app_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/opt_args/code_constructs/intext_app/intext_app_lib.mli:2: ?min

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================
/tmp/docs/opt_args/code_constructs/intext_app/intext_app_lib.ml:2: ?min

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/opt_args/code_constructs/intext_app'
```

As expected, the analyzer does not report `?max` anymore.

## Fixing the reports on `?min`

`?min` is always used externally and never used internally. We can strengthen
the signature of the exported function and split the `max` function in 2 :
- `bounded_max` which expects a mandatory labelled argument `~min`;
- `max` which only expects 2 arguments.

Code:
```OCaml
(* intext_app_lib.mli *)
val bounded_max : min:'a -> 'a -> 'a -> 'a
val min : 'a -> 'a -> 'a
```
```OCaml
(* intext_app_lib.ml *)
let max x y = if x > y then x else y

let bounded_max ~min x y =
  let res = max x y in
  max min res

let min x y  = if x < y then x else y

let bounded_min ~max x y =
  let res = min x y in
  min max res

let () =
  let max = max 0 42 in
  let min = bounded_min ~max 100 200 in
  assert (min = max)
```
```OCaml
(* intext_app_bin.ml *)
let () =
  let open Intext_app_lib in
  let min = min 0 42 in
  let max = bounded_max ~min 100 200 in
  assert (min = max)
```

`max` is only used internally so we do not export it.

Compile and analyze:
```
$ make -C intext_app
make: Entering directory '/tmp/docs/opt_args/code_constructs/intext_app'
ocamlopt -bin-annot intext_app_lib.mli intext_app_lib.ml intext_app_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/opt_args/code_constructs/intext_app'
```

The analyzer does not report anything. Our work here is done.
