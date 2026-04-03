# Table of contents

+ [Optional arguments](#optional-arguments)
    + [Definitions](#definitions)
    + [Compiler warnings](#compiler-warnings)
        + [Warning 16: unerasable-optional-argument](#warning-26-unerasable-optional-argument)
    + [Usage](#usage)
        +[Optional arguments always used](#optional-arguments-always-used)
        +[Optional arguments never used](#optional-arguments-never-used)
+ [Limitations](#limitations)
    + [Relaying](#relaying)

# Optional arguments

## Definitions

An **optional parameter** is a labeled parameter defined by prefixing the label
with `?`.

An **optional argument** is the value given (explicitly or implicitly) to an
optional parameter. Depending on the value, an optional argument is either used
or discarded.

A **use** is either :
- Explicitly providing a value.
  E.g.
  ```OCaml
  let x_or_default ?x default =
    match x with
    | Some x -> x
    | None -> default

  let _ = x_or_default ~x:true false
  let _ = x_or_default ?x:(Some true) false
  ```
  The optional argument `?x` is provided `true` explicitly in two different
  ways: `~x:true` and `?x:(Some true)`.
- Relaying an optional argument.
  E.g.
  ```OCaml
  let x_or_default ?x default =
    match x with
    | Some x -> x
    | None -> default

  let add_x_or_zero ?x n =
    let x = x_or_default ?x 0 in
    n + x
  ```
  The optional argument `?x` of `x_or_default` is used in `add_x_or_zero` by
  transferring the latter function's argument's value to the former.
- A requirement for that argument to exist.
  E.g.
  ```OCaml
  let list_map ?f l =
    match f with
    | None -> l
    | Some f -> List.map f l

  let super_map ~map ?f l = map ?f l

  let _ = super_map ~map:list_map []
  ```
  The optional argument `?f` of `list_map` is used by requirement in
  `super_map ~map:list_map []`, because the type of the `~map` parameter
  of `super_map` is `?f:'a -> 'b -> 'c`. If `list_map` did not expect an
  optional argument `?f`, then the compilation would fail with an error like :
  ```
  File "requirement.ml", line 8, characters 23-31:
  8 | let _ = super_map ~map:list_map []
                             ^^^^^^^^
  Error: This expression has type ('a -> 'a) option -> 'a list -> 'a list
         but an expression was expected of type ?f:'b -> 'c -> 'd
  ```

> [!WARNING]
> The use by relay may lead to more complex resolutions than desired. Thus, the
> definition of use may change in the future and affect the reports.
> See the [Relaying | Limitations](#relaying) for more information

A **discard** is either :
- Explicitly passing `None`.
  E.g.
  ```OCaml
  let x_or_default ?x default =
    match x with
    | Some x -> x
    | None -> default

  let _ = x_or_default ?x:None false
  ```
  The optional argument `?x` is discarded by `?x:None`.
- Not providing any value while totally applying the function :
  E.g.
  ```OCaml
  let x_or_default ?x default =
    match x with
    | Some x -> x
    | None -> default

  let _ = x_or_default false
  ```
  The optional argument `?x` is discarded in the call `x_or_default false`
  because the function is totally applied and the argument is not provided any
  value.

> [!NOTE]
> Setting default values for optional parameters does not change the definitions
> of use and discard for the optional arguments.

## Compiler warnings

The analyzer reports always and never used optional arguments. The compiler does
not have such warnings.

> [!TIP]
> To obtain a list of available compiler warnings, use
> `ocamlopt -warn-help`

Although the compiler does not have warnings related to always/never used
optional arguments, it has a relevant warning : the 16.

### Warning 16: unerasable-optional-argument

This warning is enabled by default.
It can be disabled by passing the `-w -16` to the compiler.

Description:
```
16 [unerasable-optional-argument] Unerasable optional argument.
```

Example:
```OCaml
(* warning16.ml *)
let f ?x = ()
```
```
$ ocamlopt warning16.ml
File "warning16.ml", line 2, characters 7-8:
1 | let f ?x = ()
           ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.
```

## Usage

Optional arguments always (resp. never) used are not reported by default.
Their reports can be activated by using the `--all` or `-Oa all` (resp.
`-On all`) command line arguments.
They can be deactivated by using the `--nothing` or `-Oa nothing` (resp.
`-On nothing`) command line arguments.
For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

## Optional arguments always used

The report section looks like:
```
.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
filepath:line: ?arg

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is `filepath:line: ?arg` with `filepath` the absolute
path to the file (`.mli` or `.ml`) where `?arg` is declared, `line` the line
index in `filepath` at which the function that declares `?arg` is declared,
and `?arg` the name of the optional argument.
There can be any number of such lines.

The expected resolution for an optional argument always used is to transform it
into a non-optional labeled argument.

> [!CAUTION]
> The analyzer tracks sperately optional arguments declared in `.ml` and in
> `.mli` files. This implies that it is possible that an optional argument is
> reported always used inside its compilation but not outside (and vice-versa).
> The examples should help understand the implication on the resolutions.

> [!IMPORTANT]
> Changing an optional argument into a mandatory one may trigger compilation
> errors. In particular when the argument was used with the `?x` syntax.

## Optional arguments never used

The report section looks like:
```
.> OPTIONAL ARGUMENTS: NEVER:
============================
filepath:line: ?arg

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is the same as for optional arguments always used.

The expected resolution for an optional argument never used is to remove it from
the function's parameters.

> [!CAUTION]
> The analyzer tracks sperately optional arguments declared in `.ml` and in
> `.mli` files. This implies that it is possible that an optional argument is
> reported never used inside its compilation but not outside (and vice-versa).
> The examples should help understand the implication on the resolutions.

In order to provide actionable reports, the analyzer does not report an optional
argument as never used if its declaring function is never fully applied.

> [!IMPORTANT]
> Removing an optional argument may trigger compilation errors.
> In particular when the argument was explicitly discarded with the
> `?x:None` syntax.

# Limitations

## Relaying

Following the current definition of use, relaying an optional argument to
a callee counts as a use for the callee's. This may lead to optional arguments
reported as always used although they are only used by relay. Because the
analyzer's tracking of use is intransitive, some complex may arise with the
caller's optional argument not being always used (or even never used), which
would lead to complex resolutions, losing the benefit of using the analyzer in
to clean up the code in the first place.

A future improvement could be to ignore optional arguments if they are used by
relay, in order to focus the reports on more easily actionable results.
A more complex but more precise improvement would be to introduce transitivety
in the optional arguments analysis, to identify when a use by relay is actually
always a use, never a use or sometimes a use. The latter case would then remove
the optional argument relayed to from the reports.

### Example

The reference files for this example are in the
[relaying](../../examples/docs/optional_arguments/limitations/relaying) directory.

The reference takes place in `/tmp/docs/optional_arguments/limitations`, which
is a copy of the [limitations](../../examples/docs/optional_arguments/limitations)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C relaying build
```

The analysis command is :
```
make -C relaying analyze
```

The compile + analyze command is :
```
make -C relaying
```

Code:
```OCaml
(* relaying_bin.ml *)
let f ?x () = ignore x

let g ?x () = f ?x ()

let () =
  f ~x:42 ();
  g ()
```

Compile and analyze:
```
$ make -C relaying
make: Entering directory '/tmp/docs/optional_arguments/limitations/relaying'
ocamlopt -bin-annot relaying_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/optional_arguments/limitations/relaying/relaying_bin.ml:2: ?x

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================
/tmp/docs/optional_arguments/limitations/relaying/relaying_bin.ml:4: ?x

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/optional_arguments/limitations/relaying'
```
The analyzer reports argument `?x` of `f` as always used but `?x` of `g` as
never used.
Looking at the code, `f`'s `?x` is used by providing an explicit value in
`~x:42` and by relay in the definition of `g`. It is never discarded.

If we removed `g`'s `?x` because it is never used, then `f`'s `?x`
would be discarded in `g` and not be always used anymore. This demonstrates the
ambiguity of the always used report in the presence of relaying.
