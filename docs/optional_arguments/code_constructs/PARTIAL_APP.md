The reference files for this example are in the
[partial\_app](../../../examples/docs/optional_arguments/code_constructs/partial_app) directory.

The reference takes place in `/tmp/docs/optional_arguments/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/optional_arguments/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C partial_app build
```

The analysis command is :
```
make -C partial_app analyze
```

The compile + analyze command is :
```
make -C partial_app
```

## First run

Code:
```OCaml
(* partial_app_bin.ml *)
let sum ?x ?y () =
  match x, y with
  | Some x, Some y -> x + y
  | Some x, None -> x
  | None, Some y -> y
  | None, None -> 0

let () =
  let x = 42 in
  let sum_x = sum ~x in
  ignore (_sum_x)
```

Before looking at the analysis results, let's look at the code.

There is one function defined `sum` with 2 optional arguments `?x` and `?y`, and
1 mandatory argument `()`. The function is called once without any mandatory
argument provided. Thus, `sum` is partially applied. The only optional argument
with a value is `?x`.
Consequently, `?x` is always used (a value is provided for each application of
`sum`). Because the function is only partially applied, `?y` is neither used nor
discarded. The type of `_sum_x` is `?y:int -> unit -> int` showing that `?y`
is still expected.

Compile and analyze:
```
$ make -C partial_app
make: Entering directory '/tmp/docs/optional_arguments/code_constructs/partial_app'
ocamlopt -bin-annot partial_app_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/optional_arguments/code_constructs/partial_app/partial_app_bin.ml:2: ?x

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/optional_arguments/code_constructs/partial_app'
```

As expected, `?x` is reported as always used and `?y` is not reported.
Fixing the report is the same as for the
[Total application](./code_constructs/TOTAL_APP.md) example.
Our work here is done.
