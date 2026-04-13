The reference files for this example are in the
[external\_app](../../../examples/docs/optional_arguments/code_constructs/external_app) directory.

The reference takes place in `/tmp/docs/optional_arguments/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/optional_arguments/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C external_app build
```

The analysis command is :
```
make -C external_app analyze
```

The compile + analyze command is :
```
make -C external_app
```

## First run

Code:
```OCaml
(* external_app_lib.mli *)
val max : ?min:'a -> 'a -> 'a -> 'a
val min : ?max:'a -> 'a -> 'a -> 'a
```
```OCaml
(* external_app_lib.ml *)
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
```
```OCaml
(* external_app_bin.ml *)
let () =
  let open External_app_lib in
  let max = max 0 42 in
  let min = min ~max 100 200 in
  assert (min = max)
```

Before looking at the analysis results, let's look at the code.

There are 2 functions defined and exported by `External_app_lib`: `max` and
`min`. They both have an optional argument, and both functions are used
externally. Neither is used internally.
Optional argument `?min` of `max` is never used. Optional argument `?max` of
`min` is always used.

Compile and analyze:
```
$ make -C external_app
make: Entering directory '/tmp/docs/opt_args/code_constructs/external_app'
ocamlopt -bin-annot external_app_lib.mli external_app_lib.ml external_app_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/opt_args/code_constructs/external_app/external_app_lib.mli:3: ?max

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================
/tmp/docs/opt_args/code_constructs/external_app/external_app_lib.mli:2: ?min

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/opt_args/code_constructs/external_app'
```

As expected, `?max` is reported always used and `?min` never used.

Because all the uses of the functions are outside their compilation unit, the
analyzer reports locations in the `.mli` and not the `.ml`. This is the opposite
of the [Internal application](./INTERNAL_APPLICATION.md) example.

## Fixing the reports

Code:
```OCaml
(* external_app_lib.mli *)
val max : 'a -> 'a -> 'a
val min : max:'a -> 'a -> 'a -> 'a
```
```OCaml
(* external_app_lib.ml *)
let max x y =
  if x > y then x else y

let min ~max x y =
  let min x y = if x < y then x else y in
  let res = min x y in
  min max res
```
```OCaml
(* external_app_bin.ml *)
let () =
  let open External_app_lib in
  let max = max 0 42 in
  let min = min ~max 100 200 in
  assert (min = max)
```

We removed `?min` from function `max` and simplified its body.
We transformed `?max` into a mandatory labelled argument `~max` in function `min`.

The signatures of the functions in the `.mli` have been changed accordingly.

Compile and analyze:
```
$ make -C external_app
make: Entering directory '/tmp/docs/opt_args/code_constructs/external_app'
ocamlopt -bin-annot external_app_lib.mli external_app_lib.ml external_app_bin.ml
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


make: Leaving directory '/tmp/docs/opt_args/code_constructs/external_app'
```

The analyzer does not report anything. Our work here is done.
