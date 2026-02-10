# Table of contents

+ [Exported Values](#exported-values)
    + [Definitions](#definitions)
    + [Usage](#usage)
+ [Examples](#examples)
    + [Single compilation unit without interface](#single-compilation-unit-without-interface)
    + [Single compilation unit with interface](#single-compilation-unit-with-interface)
    + [Multiple compilation units](#multiple-compilation-units)

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

## Single compilation unit without interface

This example illustrates a simple case of a compilation unit without `.mli` and
without any external use.

The reference file for this example is [`hello_world_no_intf.ml`](./hello_world_no_intf.ml).

The compilation command to produce the `hello_world_no_intf.cmi` and
`hello_world_no_intf.cmt` is :
```
ocamlopt -bin-annot hello_world_no_intf.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world_no_intf.cmi hello_world_no_intf.cmt
```

### First run

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

### Fixing the warning 26

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
/tmp/docs/exported_values/hello_world_no_intf.ml:3: goodbye

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The compiler does not report any unused value.

The analyzer, however, detects that `goodbye` declared at line 3 is now unused.

Like we did with the warning 26, `goodbye` can be removed.

### Removing the unused `goodbye`

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

## Single compilation unit with interface

This example illustrates a simple case of a compilation unit with `.mli` and
without any external use.
This is the same as the previous example with an extra interface.

The reference files for this example are [`hello_world.mli`](./hello_world.mli)
and [`hello_world.ml`](./hello_world.ml)

The compilation command to produce the `hello_world.cmi` and `hello_world.cmt` is :
```
ocamlopt -bin-annot hello_world.mli hello_world.ml
```

The analysis command is :
```
dead_code_analyzer --nothing -E all hello_world.cmi hello_world.cmt
```

### First run

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
/tmp/docs/exported_values/hello_world.mli:2: hello
/tmp/docs/exported_values/hello_world.mli:3: goodbye
/tmp/docs/exported_values/hello_world.mli:4: world

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

### Removing the unused values

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

## Multiple compilation units

This example illustrates a simple case of a library used by a binary.
This is the same as the previous example with an extra indirection.

The reference files for this example are
[`hello_world_lib.mli`](./hello_world_lib.mli),
[`hello_world_lib.ml`](./hello_world_lib.ml), and
[`hello_world_bin.ml`](./hello_world_bin.ml)

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

### First run

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

### Fixing the warning 26

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
/tmp/docs/exported_values/hello_world_lib.mli:3: goodbye

Nothing else to report in this section
--------------------------------------------------------------------------------
```

The compiler does not report any unused value.

The analyzer, however, detects that `goodbye` declared in `hello_world_lib.mli`
at line 3 is now unused.

Like we did with the warning 26, `goodbye` can be removed.

### Unexporting `goodbye`

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
