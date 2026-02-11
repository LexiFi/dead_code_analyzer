# Table of contents

+ [Exported Values](#exported-values)
    + [Definitions](#definitions)
    + [Usage](#usage)
+ [Examples](#examples)
    + [Hello world](#hello-world)
        + [Single compilation unit without interface](#single-compilation-unit-without-interface)
        + [Single compilation unit with interface](#single-compilation-unit-with-interface)
        + [Multiple compilation units](#multiple-compilation-units)
        + [All Together](#all-together)

# Exported Values

## Definitions

A **value** is the result of an expression. For the analyzer, we'll restrict
this definition to a _named_ value, i.e. a name bound to an expression.
In general, this is the `lhs` of `let lhs = ...` or `val lhs : ...`.

An **exported** value is one that is accessible outside its compilation unit.
I.e. a value that can be referenced in other `.ml` and/or `.mli` files than
the ones that declare it.

> [!NOTE]
> The _exported_ attribute is important here.
> The compiler already reports unused _unexported_ values (warning 32
> `unused-value-declaration`) and unused _local_
> values (warning 26 `unused-var`) [^unused-var-strict]. Because the compiler
> already reports these 2 categories of values, the `dead_code_analyzer`
> complements its work by focusing on the 3rd category.

[^unused-var-strict]: The compiler also has the warning 27 `unused-var-strict`
for unused values not bound by `let` or `as` (e.g. function parameters or values
in patterns). These are out of context for the analyzer.

A **use** is either :
- An explicit reference.
  E.g.
  ```OCaml
  let answer = 42
  let () = print_int answer
  ```
  The value `answer` is explicitly referenced in `print_int answer`.
- A requirement for that value to exist.
  E.g.
  ```OCaml
  module M = struct
    let answer = 42
  end

  module M2 : sig val answer : int end = M
  ```
  There are 2 values here : `M.answer`, and `M2.answer`.
  Neither of them is explictly referenced. However, `M.answer` is used by
  requirement in `M2 : ... = M`. I.e. it needs to be provided by `M` to fulfill
  `M2`'s signature. Without it the compilation would fail with an error like :
  ```
  File "requirement.ml", line 4, characters 39-40:
  4 | module M2 : sig val answer : int end = M
                                             ^
  Error: Signature mismatch:
         Modules do not match:
           sig end
         is not included in
           sig val answer : int end
         The value answer is required but not provided
         File "requirement.ml", line 4, characters 16-32: Expected declaration
  ```

## Usage

Unused exported values are reported by default.
Their reports can be deactivated by using the `--nothing` or `-E nothing`
command line arguments.
They can be reactivated by using the `--all` or `-E all` command line arguments.
For more detail on the command line arguments see [the more general Usage
documentation](../USAGE.md).

The report section title is `.> UNUSED EXPORTED VALUES:`.

The expected resolution for an unused exported value is to remove it from the
`.mli` if there is one, or the `.ml` otherwise.

> [!IMPORTANT]
> Removing unused values from the codebase (reported by either the compiler or
> the analyzer) may trigger the detection of new unused values for both the
> compiler and the analyzer. Consequently, it is expected that a user might need
> to compile and analyze their code multiple times when cleaning up their
> codebase.

In order to provide actionable reports, the analyzer does not track the same
uses depending on whether a `.mli` exists or not :
- If a value is declared in a `.mli`, then only uses outside its compilation
unit are tracked (by default).
- If there is no `.mli`, then uses inside the compilation unit are also tracked.

With that same goal in mind, the analyzer does not report _transitively_ unused
exported values. I.e. if a value is only used by unused values, then it will
not be reported as unused. It would be reported unused only after all the code
using it has been removed.
This is also the compiler's behavior for its warnings about unused values.

# Examples

## Hello world

All of the following examples can be found in the [hello\_world](./hello_world)
directory.

The reference takes place in `/tmp/docs/exported_values/hello_world`, which
contains copies of the examples. Reported locations may differ depending on the
location of the source files.

### Single compilation unit without interface

This example illustrates a simple case of a compilation unit without `.mli` and
without any external use.

The reference file for this example is
[`hello_world_no_intf.ml`](./hello_world/hello_world_no_intf.ml).

The compilation command to produce the `hello_world_no_intf.cmi` and
`hello_world_no_intf.cmt` is :
```
ocamlopt -bin-annot hello_world_no_intf.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world_no_intf.cmi hello_world_no_intf.cmt
```

#### First run

Code :
```OCaml
(* hello_world_no_intf.ml *)
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
$ ocamlopt -bin-annot hello_world_no_intf.ml
File "hello_world_no_intf.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

$ dead_code_analyzer --nothing -E all hello_world_no_intf.cmi hello_world_no_intf.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Here we can see the warning 26 is triggered during the compilation. Hence, the
_local_ value `goodbye_world` at line 8 can be removed.

The analyzer reports that there is no unused _exported_ value. The 3 exported
values are `hello`, `goodbye` and `world`. They are all referenced internally.
Because there is no `hello_world_no_intf.mli`, the internal uses are accounted for.

#### Fixing the warning 26

Let's remove the unused `goodbye_world`.

Code :
```OCaml
(* hello_world_no_intf.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"

let () =
  let hello_world = hello ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_no_intf.ml

$ dead_code_analyzer --nothing -E all hello_world_no_intf.cmi hello_world_no_intf.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/hello_world/hello_world_no_intf.ml:3: goodbye

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The compiler does not report any unused value.

The analyzer, however, detects that `goodbye` declared at line 3 is now unused.

Like we did with the warning 26, `goodbye` can be removed.

#### Removing the unused `goodbye`

Code :
```OCaml
(* hello_world_no_intf.ml *)
let hello = "Hello"
let world = "World"

let () =
  let hello_world = hello ^ world in
  print_endline hello_world
```

Compile and analyze :
```
$ ocamlopt -bin-annot hello_world_no_intf.ml

$ dead_code_analyzer --nothing -E all hello_world_no_intf.cmi hello_world_no_intf.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Now, neither the compiler nor the analyzer report any unused value. Our work here is done.

### Single compilation unit with interface

This example illustrates a simple case of a compilation unit with `.mli` and
without any external use.
This is the same as the previous example with an extra interface.

The reference files for this example are
[`hello_world.mli`](./hello_world/hello_world.mli) and
[`hello_world.ml`](./hello_world/hello_world.ml)

The compilation command to produce the `hello_world.cmi` and `hello_world.cmt` is :
```
ocamlopt -bin-annot hello_world.mli hello_world.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world.cmi hello_world.cmt
```

#### First run

Code :
```OCaml
(* hello_world.mli *)
val hello : string
val goodbye : string
val world : string
```
```OCaml
(* hello_world.ml *)
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
$ ocamlopt -bin-annot hello_world.ml
File "hello_world.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

$ dead_code_analyzer --nothing -E all hello_world.cmi hello_world.cmt
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/hello_world/hello_world.mli:2: hello
/tmp/docs/exported_values/hello_world/hello_world.mli:3: goodbye
/tmp/docs/exported_values/hello_world/hello_world.mli:4: world

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Like in the previous example, the warning 26 is triggered during the
compilation. Hence, the _local_ value `goodbye_world` at line 8 can be removed.

The analyzer reports that there are 3 unused _exported_ value in
`hello_world.mli`: `hello` at line 2, `goodbye` line 3, and `world` line 4.
These are the only exported values in the `hello_world` compilation unit because
they are the only one listed in the `.mli`. They are all used in
`hello_world.ml`, but not outside of their compilation unit. Because there is an
interface file available, only external uses are accounted for. Thus, they are
considered unused and can be dropped from the `.mli`

#### Removing the unused values

Let's remove the unused `goodbye_world` from `hello_world.ml`, reported unused
by the compiler, and the values in `hello_world.mli` reported by the analyzer.

Code :
```OCaml
(* hello_world.mli *)
```
```OCaml
(* hello_world.ml *)
let hello = "Hello"
let goodbye = "Goodbye"
let world = "World"

let () =
  let hello_world = hello ^ world in
  print_endline hello_world
```
Compile and analyze :
```
$ ocamlopt -bin-annot hello_world.ml
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
the previous example). This warning is actually off by default and can be
activated by passing the `-w +32` argument to the compiler :
```
$ ocamlopt -w +32 hello_world.ml
File "hello_world.ml", line 3, characters 4-11:
3 | let goodbye = "Goodbye"
        ^^^^^^^
Warning 32 [unused-value-declaration]: unused value goodbye.
```
The `goodbye` value can be safely removed and neither the compiler nor the
analyzer will report unused values anymore. Our work here is done.

### Multiple compilation units

This example illustrates a simple case of a library used by a binary.
This is the same as the previous example with an extra indirection.

The reference files for this example are
[`hello_world_lib.mli`](./hello_world/hello_world_lib.mli),
[`hello_world_lib.ml`](./hello_world/hello_world_lib.ml), and
[`hello_world_bin.ml`](./hello_world/hello_world_bin.ml)

The compilation command to produce the the necessary `.cmi` and `.cmt` files is :
```
ocamlopt -bin-annot hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world_lib.cmi hello_world_lib.cmt hello_world_bin.cmi hello_world_bin.cmt
```

> [!NOTE]
> It is left as an exercise to the user to explore this example without
> `hello_world_lib.mli`.

#### First run

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

Like in the previous example, the warning 26 is triggered during the
compilation. Hence, the _local_ value `goodbye_world` at line 8 can be removed.

The analyzer reports that there is no unused _exported_ value. The 3 exported
values are `hello`, `goodbye` and `world` in `hello_world_lib.mli`. They are all
referenced externally, in `hello_world_bin.ml`.

> [!NOTE]
> All the different flavors of explicit reference are taken into account the
> same way. Here, the values are referenced after a local `open`. They could
> have been referenced after a global `open` or using their full paths (e.g.
> `Hello_world_lib.hello`) without making any difference on the reports.

#### Fixing the warning 26

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

The analyzer, however, detects that `goodbye` declared in `hello_world_lib.mli`
at line 3 is now unused.

Like we did with the warning 26, `goodbye` can be removed.

#### Unexporting `goodbye`

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

### All Together

This example illustrates a simple case of a complete codebase with independent
and dependent components, and duplicate names.
This is the grouping of all the previous [*Hello World*](#hello-world) examples.
Analyzing all the files at once reduces the number of iterations to reach a
satisfying codebase.

The reference files for this example are all those listed previously.

The compilation command to produce the the necessary `.cmi` and `.cmt` files,
and the desired warnings is the combination of all the previous ones :
```
ocamlopt -w +32 -bin-annot hello_world_no_intf.ml hello_world.mli hello_world.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
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
> As we can see for the compilation command, there is a large number of files to
> list. Instead of listing all the `.cmi` and `.cmt` files in the command line,
> the analyzer accepts directories as arguments and will analyze all the
> relevant files it can find in them and in their subdirectories.

The code is not re-exposed at each iteration here. It is the same as in the previous examples.

#### First Run

Compile :
```
$ ocamlopt -w +32 -bin-annot hello_world_no_intf.ml hello_world.mli hello_world.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
File "hello_world_no_intf.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

File "hello_world.ml", line 8, characters 6-19:
8 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.

File "hello_world_bin.ml", line 5, characters 6-19:
5 |   let goodbye_world = goodbye ^ world in
          ^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable goodbye_world.
```

Without any surprise, the compiler identifies the different warning 26 explored
in the previous examples.
Let's fix them and **recompile**, before running the analyzer.

Analyze :
```
$ dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/hello_world/hello_world.mli:2: hello
/tmp/docs/exported_values/hello_world/hello_world.mli:3: goodbye
/tmp/docs/exported_values/hello_world/hello_world.mli:4: world
/tmp/docs/exported_values/hello_world/hello_world_lib.mli:3: goodbye
/tmp/docs/exported_values/hello_world/hello_world_no_intf.ml:3: goodbye

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The analyzer correclty identifies the unused exported values it identified for
the previous examples, all in one run. Note that the reports are in the
lexicographical order.
Unlike the compiler which listed its warnings in the order the files appeared in
the command line, the analyzer always sorts its reports in this order.

#### Removing the unused exported values

Like we did, in the previous examples, we can remove the exported values at the
locations reported by the analyzer.

Compile :
```
$ ocamlopt -w +32 -bin-annot hello_world_no_intf.ml hello_world.mli hello_world.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml
File "hello_world.ml", line 3, characters 4-11:
3 | let goodbye = "Goodbye"
        ^^^^^^^
Warning 32 [unused-value-declaration]: unused value goodbye.

File "hello_world_lib.ml", line 3, characters 4-11:
3 | let goodbye = "Goodbye"
        ^^^^^^^
Warning 32 [unused-value-declaration]: unused value goodbye.
```

Once again, the finidings are the same as for the previous examples. After
`goodbye_world` is removed and `goodbye` is unexported, the compiler warning 32
indicates that it is unused.
Let's fix the warnings.

Compile and analyze :
```
$ ocamlopt -w +32 -bin-annot hello_world_no_intf.ml hello_world.mli hello_world.ml hello_world_lib.mli hello_world_lib.ml hello_world_bin.ml

$ dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------
```

Now, neither the compiler nor the analyzer report any unused value. Our work here is done.
