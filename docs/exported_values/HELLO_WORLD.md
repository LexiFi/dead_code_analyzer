# Table of contents

+ [Single compilation unit without interface](#single-compilation-unit-without-interface)
+ [Single compilation unit with interface](#single-compilation-unit-with-interface)
+ [Multiple compilation units](#multiple-compilation-units)
+ [All Together](#all-together)

All of the following examples can be found in the [hello\_world](../../examples/docs/exported_values/hello_world)
directory.

The reference takes place in `/tmp/docs/exported_values/hello_world`, which
contains copies of the examples. Reported locations may differ depending on the
location of the source files.

# Single compilation unit without interface

This example illustrates a simple case of a compilation unit without `.mli` and
without any external use.

The reference file for this example is
[`hello_world_without_intf.ml`](../../examples/docs/exported_values/hello_world/hello_world_without_intf.ml).

The compilation command to produce `hello_world_without_intf.cmi` and
`hello_world_without_intf.cmt` is :
```
ocamlopt -bin-annot hello_world_without_intf.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world_without_intf.cmi hello_world_without_intf.cmt
```

## First run

Code :
```OCaml
(* hello_world_without_intf.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"

let () =
  let hello_world = hello ^ world in
  let goodbye_world = goodbye ^ world in
  print_endline hello_world
```

Compile :
```
$ ocamlopt -bin-annot hello_world_without_intf.ml
File "hello_world_without_intf.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.
```

The compiler reports a warning 26 on `goodbye_world`:
`Warning 26 [unused-var]: unused variable goodbye_world.`
This tells us that the _local_ value is unused, and, thus, can be removed at the
reported location: `File "hello_world_without_intf.ml", line 8`

Analyze :
```
$ dead_code_analyzer --nothing -E all hello_world_without_intf.cmi hello_world_without_intf.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The analyzer does not report any unused _exported_ value. There are 3 exported
in the `Hello_world_without_intf` compilation unit : `hello`, `goodbye` and `world`.
These are the top level values of `hello_world_without_intf.ml`.
They are all referenced internally. Because there is no `hello_world_without_intf.mli`,
the internal uses are accounted for. Consequently, none of the exported values
are considered unused by the analyzer.

## Fixing the warning 26

Let's remove the unused `goodbye_world`.

Code :
```OCaml
(* hello_world_without_intf.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"

let () =
  let hello_world = hello ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_without_intf.ml

$ dead_code_analyzer --nothing -E all hello_world_without_intf.cmi hello_world_without_intf.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/hello_world/hello_world_without_intf.ml:3: goodbye

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The compiler does not report any unused value.

The analyzer reports that `goodbye` declared at line 3 is unused :
`/tmp/docs/exported_values/hello_world/hello_world_without_intf.ml:3: goodbye`
Like the warning 26 above, this report tells us that `goodbye` can be removed
at the reported location.

## Removing the unused `goodbye`

Code :
```OCaml
(* hello_world_without_intf.ml *)
let hello = "Hello"
let world = "World"

let () =
  let hello_world = hello ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_without_intf.ml

$ dead_code_analyzer --nothing -E all hello_world_without_intf.cmi hello_world_without_intf.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Now, neither the compiler nor the analyzer report any unused value.
Our work here is done.

# Single compilation unit with interface

This example is the same as the previous example, with an explicit `.mli`.
Although an interface is provided, all the uses remain inside the same
compilation unit.

The reference files for this example are
[`hello_world_with_intf.mli`](../../examples/docs/exported_values/hello_world/hello_world_with_intf.mli) and
[`hello_world_with_intf.ml`](../../examples/docs/exported_values/hello_world/hello_world_with_intf.ml)

The compilation command to produce `hello_world.cmi` and `hello_world.cmt` is :
```
ocamlopt -bin-annot hello_world_with_intf.mli hello_world_with_intf.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world.cmi hello_world.cmt
```

## First run

Code :
```OCaml
(* hello_world_with_intf.mli *)
val hello : string
val goodbye : string
val world : string
```
```OCaml
(* hello_world_with_intf.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"

let () =
  let hello_world = hello ^ world in
  let goodbye_world = goodbye ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_with_intf.ml
File "hello_world_with_intf.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

$ dead_code_analyzer --nothing -E all hello_world.cmi hello_world.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/hello_world/hello_world_with_intf.mli:2: hello
/tmp/docs/exported_values/hello_world/hello_world_with_intf.mli:3: goodbye
/tmp/docs/exported_values/hello_world/hello_world_with_intf.mli:4: world

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The compiler reports the same warning 26 on `goodbye_world` as in the previous
example.

The analyzer reports that there are 3 unused _exported_ value in
`hello_world_with_intf.mli`: `hello` at line 2, `goodbye` line 3, and `world` line 4.
These are the only exported values in the `Hello_world_with_intf` compilation
unit because they are the only ones listed in the `.mli`. They are all used in
`hello_world_with_intf.ml`, but not outside of their compilation unit. Because there is an
interface file available, only external uses are accounted for. Thus, they are
considered unused and can be dropped from the `.mli`

## Removing the unused values

Let's remove the unused `goodbye_world` from `hello_world_with_intf.ml`, reported unused
by the compiler, and the values in `hello_world_with_intf.mli` reported by the analyzer.

Code :
```OCaml
(* hello_world_with_intf.mli *)
```
```OCaml
(* hello_world_with_intf.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"

let () =
  let hello_world = hello ^ world in
  print_endline hello_world
```
Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_with_intf.ml
$ dead_code_analyzer --nothing -E all hello_world.cmi hello_world.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Now, neither the compiler nor the analyzer report any unused value.

We learned from the previous example that the `goodbye` value is unused after
we remove `goodbye_world`. This is on the compiler to report it in this example
because it is an _unexported_ value here (while it was an _exported_ value in
the previous example). The corresponding warning is actually off by default and
can be activated by passing the `-w +32` argument to the compiler :
```
$ ocamlopt -w +32 hello_world_with_intf.ml
File "hello_world_with_intf.ml", line 3, characters 4-11:
3 | let goodbye = "Goodbye"
        ^^^^^^^
Warning 32 [unused-value-declaration]: unused value goodbye.
```
The `goodbye` value can be safely removed and neither the compiler nor the
analyzer will report unused values anymore. Our work here is done.

# Multiple compilation units

This example is the same as the previous example, split in 2 separate
compilation units. All the exported values are now used externally.

The reference files for this example are
[`hello_world_lib.mli`](../../examples/docs/exported_values/hello_world/hello_world_lib.mli),
[`hello_world_lib.ml`](../../examples/docs/exported_values/hello_world/hello_world_lib.ml), and
[`hello_world_bin.ml`](../../examples/docs/exported_values/hello_world/hello_world_bin.ml)

The compilation command to produce the necessary `.cmi` and `.cmt` files is :
```
ocamlopt -bin-annot hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world_lib.cmi hello_world_lib.cmt hello_world_bin.cmi hello_world_bin.cmt
```

> [!NOTE]
> It is left as an exercise to the reader to explore this example without
> `hello_world_lib.mli`.

## First run

Code :
```OCaml
(* hello_world_lib.mli *)
val hello : string
val goodbye : string
val world : string
```
```OCaml
(* hello_world_lib.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"
```
```OCaml
(* hello_world_bin.ml *)
let () =
  let open Hello_world_lib in
  let hello_world = hello ^ world in
  let goodbye_world = goodbye ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
File "hello_world_bin.ml", line 5, characters 6-19:
5 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

$ dead_code_analyzer --nothing -E all hello_world_lib.cmi hello_world_lib.cmt hello_world_bin.cmi hello_world_bin.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The compiler reports the same warning 26 on `goodbye_world` as in the previous
example.

The analyzer does not report any unused _exported_ value. The 3 exported
values are `hello`, `goodbye` and `world` in `hello_world_lib.mli`. They are all
referenced externally, in `hello_world_bin.ml`.

> [!NOTE]
> All the different flavors of explicit reference are taken into account the
> same way. Here, the values are referenced after a local `open`. They could
> have been referenced after a global `open` or using their full paths (e.g.
> `Hello_world_lib.hello`) without making any difference on the reports.

## Fixing the warning 26

Let's remove the unused `goodbye_world`.

Code :
```OCaml
(* hello_world_lib.mli *)
val hello : string
val goodbye : string
val world : string
```
```OCaml
(* hello_world_lib.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"
```
```OCaml
(* hello_world_bin.ml *)
let () =
  let open Hello_world_lib in
  let hello_world = hello ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml

$ dead_code_analyzer --nothing -E all hello_world_lib.cmi hello_world_lib.cmt hello_world_bin.cmi hello_world_bin.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/hello_world/hello_world_lib.mli:3: goodbye

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The compiler does not report any unused value.

The analyzer reports `goodbye` as unused, as in the previous example.

## Unexporting `goodbye`

Code :
```OCaml
(* hello_world_lib.mli *)
val hello : string
val world : string
```
```OCaml
(* hello_world_lib.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"
```
```OCaml
(* hello_world_bin.ml *)
let () =
  let open Hello_world_lib in
  let hello_world = hello ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml

$ dead_code_analyzer --nothing -E all hello_world_lib.cmi hello_world_lib.cmt hello_world_bin.cmi hello_world_bin.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Now, neither the compiler nor the analyzer report any unused value.

Like in the previous example, we need to pass `-w +32` to the compiler to
trigger the `unused-value-declaration` warning :
```
$ ocamlopt -bin-annot hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
File "hello_world_lib.ml", line 3, characters 4-11:
3 | let goodbye = "Goodbye"
        ^^^^^^^
Warning 32 [unused-value-declaration]: unused value goodbye.
```
The `goodbye` value can be safely removed and neither the compiler nor the
analyzer will report unused values anymore. Our work here is done.

# All Together

This example is the grouping of all the previous [*Hello World*](#hello-world)
examples. Analyzing all the files at once reduces the number of iterations to
reach a satisfying codebase.

The reference files for this example are all those listed previously.

The compilation command to produce the necessary `.cmi` and `.cmt` files,
and the desired warnings is the combination of all the previous ones :
```
ocamlopt -w +32 -bin-annot hello_world_without_intf.ml hello_world_with_intf.mli hello_world_with_intf.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
```

> [!NOTE]
> For our usage, this has the same effect has running each of the previous
> compliation commands, with the extra `-w +32` argument, one after the other.
> The benefit is that all the warnings will be printed at once.

The analysis command is :
```
dead_code_analyzer --nothing -E all .
```

> [!TIP]
> As we can see in the compilation command, there is a large number of files to
> list. Instead of listing all the `.cmi` and `.cmt` files in the command line,
> the analyzer accepts directories as arguments and will analyze all the
> relevant files it can find in them.

The code is not re-exposed at each iteration here. It is the same as in the
previous examples.

## First Run

Compile :
```
$ ocamlopt -w +32 -bin-annot hello_world_without_intf.ml hello_world_with_intf.mli hello_world_with_intf.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
File "hello_world_without_intf.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

File "hello_world_with_intf.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

File "hello_world_bin.ml", line 5, characters 6-19:
5 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.
```

Without any surprise, the compiler reports the warnings 26 explored
in the previous examples.
Let's fix them and **recompile**, before running the analyzer.

Analyze :
```
$ dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/hello_world/hello_world_with_intf.mli:2: hello
/tmp/docs/exported_values/hello_world/hello_world_with_intf.mli:3: goodbye
/tmp/docs/exported_values/hello_world/hello_world_with_intf.mli:4: world
/tmp/docs/exported_values/hello_world/hello_world_lib.mli:3: goodbye
/tmp/docs/exported_values/hello_world/hello_world_without_intf.ml:3: goodbye

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The analyzer correctly reports the unused exported values it reported in
the previous examples, all in one run. Note that the reports are in the
lexicographical order.
Unlike the compiler which listed its warnings in the order of the files in
the command line, the analyzer always sorts its reports in this order.

## Removing the unused exported values

Like we did, in the previous examples, we can remove the exported values at the
locations reported by the analyzer.

Compile :
```
$ ocamlopt -w +32 -bin-annot hello_world_without_intf.ml hello_world_with_intf.mli hello_world_with_intf.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
File "hello_world_with_intf.ml", line 3, characters 4-11:
3 | let goodbye = "Goodbye"
        ^^^^^^^
Warning 32 [unused-value-declaration]: unused value goodbye.

File "hello_world_lib.ml", line 3, characters 4-11:
3 | let goodbye = "Goodbye"
        ^^^^^^^
Warning 32 [unused-value-declaration]: unused value goodbye.
```

Once again, the warnings are the same as for the previous examples. After
`goodbye_world` is removed and `goodbye` is unexported, the compiler warning 32
indicates that `goodbye` is unused.
Let's fix the warnings.

Compile and analyze :
```
$ ocamlopt -w +32 -bin-annot hello_world_without_intf.ml hello_world_with_intf.mli hello_world_with_intf.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml

$ dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Now, neither the compiler nor the analyzer report any unused value.
Our work here is done.
