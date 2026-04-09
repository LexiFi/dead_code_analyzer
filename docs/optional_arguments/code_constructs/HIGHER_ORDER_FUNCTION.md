The reference files for this example are in the
[hof](../../../examples/docs/optional_arguments/code_constructs/hof) directory.

The reference takes place in `/tmp/docs/optional_arguments/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/optional_arguments/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C hof build
```

The analysis command is :
```
make -C hof analyze
```

The compile + analyze command is :
```
make -C hof
```

## First run

Code:
```OCaml
(* hof_bin.ml *)
let map_with_index (f : ?index:int -> int -> 'a) l =
  let f' index = f ~index in
  List.mapi f' l

let add_index ?(index = 0) x =
  x + index

let add_index l =
  map_with_index add_index l
```

Before looking at the analysis results, let's look at the code.

There are 3 functions defined at the top level :
- `map_with_index` which takes a function as argument (`f`) and a list (`l`).
  `f` expects an optional argument `?index` and an `int`.
- `add_index` which takes an optional argument `?index` and an `int`
- `add_index` which takes a list as argument. This definition shadows the previous one

In the last `add_index`, the first `add_index` is passed as argument to
`map_with_index`. Thus, `add_index`'s `?index` is used because it is required by
the signature of `map_with_index`. The first `add_index` is not used anywhere
else, so its `?index` optional argument is always used.

Within `map_with_index`, `f`'s `?index` is always used. The analyzer tracks all
optional arguments uses, regardless of the kind of their defining functions
(exported, local, recursive, ...). Thus, it should report the parameter `f`'s
`?index` as always used.

Compile and analyze:
```
$ make -C hof
make: Entering directory '/tmp/docs/optional_arguments/code_constructs/hof'
ocamlopt -bin-annot hof_bin.ml
dead_code_analyzer --nothing -Oa all -On all .
Scanning files...
 [DONE]

.> OPTIONAL ARGUMENTS: ALWAYS:
=============================
/tmp/docs/optional_arguments/code_constructs/hof/hof_bin.ml:2: ?index
/tmp/docs/optional_arguments/code_constructs/hof/hof_bin.ml:6: ?index

Nothing else to report in this section
--------------------------------------------------------------------------------


.> OPTIONAL ARGUMENTS: NEVER:
============================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/optional_arguments/code_constructs/hof'
```

As expected, the analyzer reports 2 `?index` as always used. The first one is
locatted at line `2` and is the one in the signature of `map_with_index`.
The second one is located at line 6 and isthe one of the first `add_index` function.

## Fixing the reports

These reports can be trivially fixed by making the optional arguments mandatory.

```OCaml
(* hof_bin.ml *)
let map_with_index (f : index:int -> int -> 'a) l =
  let f' index = f ~index in
  List.mapi f' l

let add_index ~index x =
  x + index

let add_index l =
  map_with_index add_index l
```

Compile and analyze:
```
$ make -C hof
make: Entering directory '/tmp/docs/optional_arguments/code_constructs/hof'
ocamlopt -bin-annot hof_bin.ml
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


make: Leaving directory '/tmp/docs/optional_arguments/code_constructs/hof'
```

The analyzer does not report anything. Our work here is done.
