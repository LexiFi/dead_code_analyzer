The reference files for this example are in the
[total\_app](../../../examples/docs/optional_arguments/code_constructs/total_app) directory.

The reference takes place in `/tmp/docs/optional_arguments/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/optional_arguments/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C total_app build
```

The analysis command is :
```
make -C total_app analyze
```

The compile + analyze command is :
```
make -C total_app
```

## First run

Code:
```OCaml
(* total_app_bin.ml *)
let sum ?x ?y () =
  match x, y with
  | Some x, Some y -> x + y
  | Some x, None -> x
  | None, Some y -> y
  | None, None -> 0

let () =
  let x = 42 in
  let x' = sum ~x () in
  assert (x' = x)
```

Before looking at the analysis results, let's look at the code.

There is one function defined: `sum` with 2 optional arguments `?x` and `?y`, and
1 mandatory argument `()`. The function is called once with all the mandatory
arguments provided. Thus, `sum` is totally applied. The only optional argument
with a value is `?x`.
Consequently, `?x` is always used (a value is provided for each application of
`sum`), and `?y` is never used (a value is never provided when applying `sum`).

Compile and analyze:
```
$ make -C total_app
make: Entering directory '/tmp/docs/optional_arguments/code_constructs/total_app'
ocamlopt -bin-annot total_app_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/optional_arguments/code_constructs/total_app/total_app_bin.ml:2: ?x

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================
/tmp/docs/optional_arguments/code_constructs/total_app/total_app_bin.ml:2: ?y

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/optional_arguments/code_constructs/total_app'
```

As expected, `?x` is reported as always used and `?y` as never used.

> [!NOTE]
> Although the reported locations are the ones where the arguments are defined,
> this is coincidental. In reality, the reported locations are those of the
> functions that declare the optional arguments.

## Fixing the always used report

Because `?x` is always used, it can be made mandatory.

Code:
```OCaml
(* total_app_bin.ml *)
let sum ~x ?y () =
  match y with
  | Some y -> x + y
  | None -> x

let () =
  let x = 42 in
  let x' = sum ~x () in
  assert (x' = x)
```

Notice how we chose to make `~x` mandatory and of `int` type while the optional
parameter `?x` was of `int option` type. Changing the type allowed us to
simplify the code of `sum`.

> [!TIP]
> Alternatively, we could have kept the same `int option` type for `~x` and
> updated its use in `sum ~x` by `sum ~x:(Some x)`, without changing the body of
> `sum`. Anyway, making the argument mandatory implies changes in either the use
> or the definition of the function. Otherwise the compiler would reject the
> code with a message like :
> ```
> File "total_app_bin.ml", line 11, characters 16-17:
> 11 |   let x' = sum ~x () in
>                      ^
> Error: This expression has type int but an expression was expected of type
>          int option
> ```

Compile and analyze:
```
$ make -C total_app
make: Entering directory '/tmp/docs/optional_arguments/code_constructs/total_app'
ocamlopt -bin-annot total_app_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================
/tmp/docs/optional_arguments/code_constructs/total_app/total_app_bin.ml:2: ?y

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/optional_arguments/code_constructs/total_app'
```

The analyzer does not report any optional argument as always used. It still
reports `?y` as never unused.

## Fixing the never used report

Because `?y` is never used, it can be removed.

Code:
```OCaml
(* total_app_bin.ml *)
let sum ~x () = x

let () =
  let x = 42 in
  let x' = sum ~x () in
  assert (x' = x)
```

With `?y` removed from the parameters, the body of `sum` can be simplified once more.

> [!TIP]
> Because there are no more optional arguments, the `unit` argument is not
> necessary anymore. For the purpose of the example, we'll leave the code as it
> is. It could not have been removed in the previous iteration, even by
> switching the positions of `~x` and `?y` because the compiler relies on the
> application of a non-labelled argument appearing after the optional argument in
> the function type to implicilty discard the optional argument.

Compile and analyze:
```
$ make -C total_app
make: Entering directory '/tmp/docs/optional_arguments/code_constructs/total_app'
ocamlopt -bin-annot total_app_bin.ml
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


make: Leaving directory '/tmp/docs/optional_arguments/code_constructs/total_app'
```

The analyzer does not report anything. Our work here is done.
