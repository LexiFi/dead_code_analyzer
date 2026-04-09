# Table of contents

+ [Coding style](#coding-style)
    + [Usage](#usage)
    + [Useless binding](#useless-binding)
        + [Example](#bind-example)
        + [Limitation](#bind-limitation)
    + [Optional arg in arg](#optional-arg-in-arg)
        + [Example](#opt-example)
        + [Limitation](#opt-limitation)
    + [Use sequence](#use-sequence)
        + [Example](#seq-example)

# Coding style

This section relates to coding patterns. Although this is not related to dead
code, the tool has been reporting style issues almost since its conception
(commit [0adbc0d](https://github.com/LexiFi/dead_code_analyzer/commit/0adbc0d504830a01d64a701572b5e7cb3b29f03e)).
Tracking stylistic issues comes for "free" during the analysis and can be handy
when cleaning up a codebase so it was kept as part of the tool.

The analyzer only reports 4 different kinds of stylistic issues :
- `bind`: useless binding
- `opt`: parameter of arrow type expecting an optional argument
- `seq`: binding to unit instead of using sequence
- `unit`: binding unit to a name

## Usage

Stylistic issues are not reported by default.
Their reports can be activated by using the `--all` or `-S +all` command line
arguments.
They can be deactivated by using the `--nothing` or `-S -all` command line
arguments.
Each of the sylistic issue category can be selectively activated/decativated as
described in their respective sections.
For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

The report section looks like:
```
.> CODING STYLE:
===============
filepath:line: issue

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is `filepath:line: issue` with `filepath` the absolute
path to the file where the `issue` lies, `line` the line index in `filepath` at
which the `issue` is, and `issue` the description of the stylistic issue.
There can be any number of such lines.

The expected resolution depends on the reported issue category. This is
described in their respective sections.

## Useless binding

A.k.a `bind`.
This issue is different from unused variables or unused values (for more
details, see the  [Exported Values](../exported_values/EXPORTED_VALUES.md)
documentation).

This stylistic issue category can be selectively activated by using the
`-S +bind` command line argument.
It can be deactivated by using the `-S -bind` command line argument.

This category targets patterns of the form:
```OCaml
let x = ... in x
```
I.e. the variable is immediately returned.

The expected resolution is to remove the intermediate binding:
```Diff
- let x =
    ...
- in x
```

### Example <a name="bind-example"></a>

The reference files for this example are in the
[bind](../../examples/docs/coding_style/bind) directory.

The reference takes place in `/tmp/docs/coding_style`, which
is a copy of the [coding\_style](../../examples/docs/coding_style)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C bind build
```

The analysis command is :
```
make -C bind analyze
```

The compile + analyze command is :
```
make -C bind
```

Code:
```OCaml
(* bind.ml *)
let v =
  let interm = 42 in
  interm
```

Compile and analyze:
```
$ make -C bind
make: Entering directory '/tmp/docs/coding_style/bind'
ocamlopt -bin-annot bind.ml
dead_code_analyzer --nothing -S +bind .
Scanning files...
 [DONE]

.> CODING STYLE:
===============
/tmp/docs/coding_style/bind/bind.ml:3: let x = ... in x (=> useless binding)

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/coding_style/bind'
```

The analyzer reports a coding style issue in `tmp/docs/coding_style/bind/bind.ml`
at line `3`. The reported issue is `let x = ... in x (=> useless binding)`, aka
a `bind` issue.

The reported location points to `let interm = 42 in`. Removing the binding to
`interm` and replacing its use by its value (`42`) fixes the issue.

Code:
```OCaml
(* bind.ml *)
let v = 42
```

### Limitation <a name="bind-limitation"></a>

`bind` issues are always reported with the same content :
`let x = ... in x (=> useless binding)`. The name of the variable is not
adapted to fit the actual name found in code (`interm` in the example above).


## Optional arg in arg

A.k.a `opt`.

This stylistic issue category can be selectively activated by using the
`-S +opt` command line argument.
It can be deactivated by using the `-S -opt` command line argument.

This category targets patterns of the form:
```OCaml
val f: ... -> (... -> ?_:_ -> ...) -> ...
```
I.e. higher-order functions expecting a function with an optional argument.

The expected resolution is to make the optional argument mandatory.

### Example <a name="opt-example"></a>

The reference files for this example are in the
[bindopt/../examples/docs/coding_style/bind) directory.

The reference takes place in `/tmp/docs/coding_style`, which
is a copy of the [coding\_style](../../examples/docs/coding_style)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C opt build
```

The analysis command is :
```
make -C opt analyze
```

The compile + analyze command is :
```
make -C opt
```

Code:
```OCaml
(* opt.ml *)
let map_with_index_on_negative (f : ?index:int -> int -> 'a) l =
  let index = ref 0 in
  let f' x =
    let res =
      if x < 0 then f ~index:(!index) x
      else f x
    in
    incr index;
    res
  in
  List.map f' l

let add_index_to_negative l =
  let add_index ?(index=0) x = x + index in
  map_with_index_on_negative add_index l
```

Compile and analyze:
```
$ make -C opt
make: Entering directory '/tmp/docs/coding_style/opt'
ocamlopt -bin-annot opt.ml
dead_code_analyzer --nothing -S +opt .
Scanning files...
 [DONE]

.> CODING STYLE:
===============
/tmp/docs/coding_style/opt/opt.ml:2: val f: ... -> (... -> ?_:_ -> ...) -> ...

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/coding_style/opt'
```

The analyzer reports a coding style issue in `/tmp/docs/coding_style/opt/opt.ml`
at line `2`. The reported issue is `val f: ... -> (... -> ?_:_ -> ...) -> ...`,
aka an `opt` issue.

The reported location points to `let map_with_index_on_negative ...` which
expects a function as argument (`f`), which itself expects an optional argument (`?index`).
Making `?index` mandatory, and updating the code accordingly, fixes the issue.

Code:
```OCaml
(* opt.ml *)
let map_with_index_on_negative f l =
  let index = ref 0 in
  let f' x =
    let res =
      if x < 0 then f ~index:(Some !index) x
      else f ~index:None x
    in
    incr index;
    res
  in
  List.map f' l

let add_index_to_negative l =
  let add_index ?(index=0) x = x + index in
  let add_index ~index = add_index ?index in
  map_with_index_on_negative add_index l
```

> [!NOTE]
> We made `~index` madatory but changed its type to an option, and added a
> redefinition of `add_index` to convert the mandatory argument into the
> optional one. This is the "easiest" and most re-appliable solution for this
> issue but not necessarily the best one. Context is key to decide on the
> refactors that should help resolve the issue.
> E.g. in this example, a possible refactor would be :
> ```OCaml
> (* opt.ml *)
> let map_with_index f l =
>   let index = ref 0 in
>   let f' x =
>     let res = f ~index:(!index) x in
>     incr index;
>     res
>   in
>   List.map f' l
>
> let add_index_to_negative l =
>   let add_index_to_negative ~index x =
>     if x < 0 then x + index
>     else x
>   in
>   map_with_index add_index_to_negative l
> ```

### Limitation <a name="opt-limitation"></a>

`opt` issues are always reported with the same content :
`val f: ... -> (... -> ?_:_ -> ...) -> ...`. The name of the function is not
adapted to fit the actual name found in code (`map_with_index_on_negative` in
the example above).

## Use sequence

A.k.a `seq`.

This stylistic issue category can be selectively activated by using the
`-S +seq` command line argument.
It can be deactivated by using the `-S -seq` command line argument.

This category targets patterns of the form:
```OCaml
let () = e1 in e2
```
I.e. binding to unit instead of using a sequence.

The expected resolution is to use a sequence instead of the intermediate binding:
```OCaml
e1;
e2
```

> [!TIP]
> If you are using this pattern to ensure `e1` is of type `unit`, the compiler
> will report a warning 10 `non-unit-statement` when using the suggested
> sequence pattern if `e1` is not `unit`. Alternatively, you may use a type
> annotation `(e1 : unit)` to enforce it and the compiler will report an error
> if `e1` is not `unit`.

### Example <a name="seq-example"></a>

The reference files for this example are in the
[seq](../../examples/docs/coding_style/seq) directory.

The reference takes place in `/tmp/docs/coding_style`, which
is a copy of the [coding\_style](../../examples/docs/coding_style)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C seq build
```

The analysis command is :
```
make -C seq analyze
```

The compile + analyze command is :
```
make -C seq
```

Code:
```OCaml
(* seq.ml *)
let compute_answer () =
  let () = print_endline "Computing answer" in
  42
```

Compile and analyze:
```
$ make -C seq
make: Entering directory '/tmp/docs/coding_style/seq'
ocamlopt -bin-annot seq.ml
dead_code_analyzer --nothing -S +seq .
Scanning files...
 [DONE]

.> CODING STYLE:
===============
/tmp/docs/coding_style/seq/seq.ml:3: let () = ... in ... (=> use sequence)

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/coding_style/seq'
```

The analyzer reports a coding style issue in `tmp/docs/coding_style/seq/seq.ml`
at line `3`. The reported issue is `let () = ... in ... (=> use sequence)`, aka
a `seq` issue.

The reported location points to `let () = print_endline "Computing answer" in`
which can be replaced by a sequence `print_endline "Computing answer;`

Code:
```OCaml
(* seq.ml *)
let compute_answer () =
  print_endline "Computing answer";
  42
```
