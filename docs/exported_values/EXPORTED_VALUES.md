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
    + [Code constructs](#code-constructs)
        + [Function](#function)
        + [Module](#module)
        + [Functor](#functor)

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
> It is left as an exercise to the reader to explore this example without
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


## Code constructs

All of the following examples can be found in the
[code\_constructs](./code_constructs) directory.

The reference takes place in `/tmp/docs/exported_values/code_constructs`, which
is a copy of `code_constructs`. Reported locations may differ depending on the
location of the source files.

### Function

This example illustrates that exported functions are exported values.

The reference files for this example are in the
[function](./code_constructs/function) directory.

The compilation command is :
```
make -C function build
```

The analysis command is :
```
make -C function analyze
```

The compile + analyze command is :
```
make -C function
```

#### First run

Code:
```OCaml
(* function_lib.mli *)

val memoize : f:('a -> 'b) -> 'a -> 'b

val heavy_computation : 'a -> 'a

val unused : 'a -> 'a

val do_nothing : 'a -> unit
```
```OCaml
(* function_lib.ml *)

let memoize ~f =
  let mem = Hashtbl.create 8 in
  function x ->
  match Hashtbl.find_opt mem x with
  | Some y -> y
  | None ->
    let y = f x in
    Hashtbl.add mem x y;
    y

let heavy_computation x = x

let unused x = x

let do_nothing x = ()
```
```OCaml
(* function_bin.ml *)

let () =
  let my_memoized = Function_lib.(memoize ~f:heavy_computation) in
  Function_lib.do_nothing ();
  assert (my_memoized 42 = my_memoized 42)
```

Function values are analyzed like any other value. Hence, passing them as
argument to a function or applying them (even partially) count as uses just like
any other explicit reference. Therefore, `Function_lib`'s `heavy_computation`,
`memoize`, and `do_nothing` are used in `Function_bin`. This leaves
`Function_lib.unused` as the only unused exported value.


Compile and analyze :
```
$ make -C function
make: Entering directory '/tmp/docs/exported_values/code_constructs/function'
ocamlopt -w +27+32 -bin-annot function_lib.mli function_lib.ml function_bin.ml
File "function_lib.ml", line 17, characters 15-16:
17 | let do_nothing x = ()
                    ^
Warning 27 [unused-var-strict]: unused variable x.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/function/function_lib.mli:7: unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/function'
```

The compiler reports that `do_nothing`'s parameter `x` is unused via a
warning 27. This can be easily fixed by prefixing the name with an underscore
`_` or even replacing the name by an underscore. Depending on the context, other
solutions may be considered, such as removing the parameter, or fixing the
argument's type to `unit`.

> [!NOTE]
> The warning 27 is off by default. It is enabled by passing the `-w +27`
> argument to the compiler

As expected, the only unused exported value reported by the analyzer is
`unused` in `function_lib.mli`.

#### Removing the unused values

The warning 27 is fixed by updating `do_nothing`'s type to `unit -> unit` and
its parameter `x` to `()`.

Code:
```OCaml
(* function_lib.mli *)

val memoize : f:('a -> 'b) -> 'a -> 'b

val heavy_computation : 'a -> 'a

val do_nothing : unit -> unit
```
```OCaml
(* function_lib.ml *)

let memoize ~f =
  let mem = Hashtbl.create 8 in
  function x ->
  match Hashtbl.find_opt mem x with
  | Some y -> y
  | None ->
    let y = f x in
    Hashtbl.add mem x y;
    y

let heavy_computation x = x

let unused x = x

let do_nothing () = ()
```
```OCaml
(* function_bin.ml *)

let () =
  let my_memoized = Function_lib.(memoize ~f:heavy_computation) in
  Function_lib.do_nothing ();
  assert (my_memoized 42 = my_memoized 42)
```

Compile and analyze :
```
$ make -C function
make: Entering directory '/tmp/docs/exported_values/code_constructs/function'
ocamlopt -w +27+32 -bin-annot function_lib.mli function_lib.ml function_bin.ml
File "function_lib.ml", line 15, characters 4-10:
15 | let unused x = x
         ^^^^^^
Warning 32 [unused-value-declaration]: unused value unused.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/function'
```

Now that `unused` is unexported, the compiler can report it as unused via the
warning 32, and the analyzer does not report anaything. Removing that value
fixes all the warnings. Our work here is done.


### Module

This example illustrates a simple case of exported values in submodules.

The reference files for this example are in the
[module](./code_constructs/module) directory.

The compilation command is :
```
make -C module build
```

The analysis command is :
```
make -C module analyze
```

The compile + analyze command is :
```
make -C module
```

#### First run

Code:
```OCaml
(* module_lib.mli *)
module M : sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end
```
```OCaml
(* module_lib.ml *)
module M = struct
  type t = unit
  let externally_used = ()
  let internally_used = ()
  let unused = ()
  let unused_unexported = ()
end

let () = M.internally_used
```
```OCaml
(* module_bin.ml *)
let () =
  ignore Module_lib.M.externally_used
```

Before looking at the analysis results, let's look at the code.
Here, all the values of `Module_lib.M` are exported except for
`unused_unexported`. Among the exported values, `unused` is not referenced
anywhere, `internally_used` is only referenced within its compilation unit
(`Module_lib`), and `externally_used` is only referenced outside of it.

> [!IMPORTANT]
> Using `internally_used` inside of `M` rather than outside would provide the
> same results. The only scope of interest is the compilation unit.

Compile and analyze :
```
$ make -C module
make: Entering directory '/tmp/docs/exported_values/code_constructs/module'
ocamlopt -w +32 -bin-annot module_lib.mli module_lib.ml module_bin.ml
File "module_lib.ml", line 7, characters 6-23:
7 |   let unused_unexported = ()
          ^^^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_unexported.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/module/module_lib.mli:5: M.internally_used
/tmp/docs/exported_values/code_constructs/module/module_lib.mli:6: M.unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/module'
```

The compiler detects that `unused_unexported` is unused.

The analyzer reports `M.internally_used` and `M.unused` as unused. Notice how
it did not only report the name of the value but its full path within its
compilation unit.

> [!NOTE]
> `M.internally_used` is only used withing its compialtion unit. Because its is
> declared in a `.mli`, only external uses are accounted for.
> It is left as an exercise to the reader to explore this example without
> `module_lib.mli`

#### Removing the unused values

Code:
```OCaml
(* module_lib.mli *)
module M : sig
  type t
  val externally_used : t
end
```
```OCaml
(* module_lib.ml *)
module M = struct
  type t = unit
  let externally_used = ()
  let internally_used = ()
  let unused = ()
end

let () = M.internally_used
```
```OCaml
(* module_bin.ml *)
let () =
  ignore Module_lib.M.externally_used
```

Compile and analyze :
```
$ make -C module
make: Entering directory '/tmp/docs/exported_values/code_constructs/module'
ocamlopt -w +32 -bin-annot module_lib.mli module_lib.ml module_bin.ml
File "module_lib.ml", line 6, characters 6-12:
6 |   let unused = ()
          ^^^^^^
Warning 32 [unused-value-declaration]: unused value unused.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/module'
```

The compiler reports `unused` as unused and the analyzer does not report anything. Removing that value fixes all the warnings. Our work here is done.

### Functor

This example illustrates a simple case of exported values in functors.

The reference files for this example are in the
[functor](./code_constructs/functor) directory.

The compilation command is :
```
make -C functor build
```

The analysis command is :
```
make -C functor analyze
```

The compile + analyze command is :
```
make -C functor
```

#### First run

Code:
```OCaml
(* functor_lib.mli *)
type t

module F (P : sig
  val used_required : t
  val unused_required : t
end) : sig
  val externally_used : t
  val internally_used : t
  val unused : t
end

module InternalParam : sig
  val used_required : t
  val unused_required : t
end

module ExternalParam : sig
  val used_required : t
  val unused_required : t
end

module InternalApp : sig
  val externally_used : t
  val internally_used : t
  val unused : t
end
```
```OCaml
(* functor_lib.ml *)
type t = unit

module F (P : sig
  val used_required : t
  val unused_required : t
end) = struct
  let externally_used = P.used_required
  let internally_used = P.used_required
  let unused = P.used_required
  let unused_unexported = P.used_required
  let () = internally_used
end

module InternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module ExternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module InternalApp = F(InternalParam)
```
```OCaml
(* functor_bin.ml *)
open Functor_lib

module ExternalApp = F(ExternalParam)

let () =
  ignore InternalApp.externally_used;
  ignore ExternalApp.externally_used
```

Before looking at the analysis results, let's look at the code.

The `Functor_lib` compilation unit exports 1 functor and 3 modules :
- `F` takes a module containing the values `used_required` and `unused_required`,
  and returns a module with 3 values whose names are explicit.
- `InternalParam` and `ExternalParam` fit the signature of `F`'s parameter `P`.
  The first one is used for a functor application inside `Functor_lib`.
  The second one is used for a functor application outside of it.
- `InternalApp` fits the signature of the result of `F`, and is impemented as
  the result of applying `F` inside its compilation unit.

The `Functor_bin` compilation unit exports 1 module : `ExternalApp`, which is
the result of applying `Functor_lib.F` outside its compilation unit.

Among all the exported values, the only explicit references accounted for are
those of `InternalApp.externally_used` and `External_app.externally_used` in
`Functor_bin`.

Additionally, some values are used by requirement. Because `InternalParam` and
`ExternalParam` are passed as arguments to `F`, their values `used_required` and
`unused_required` are used by requirement to fulfill the signature of `F`'s
parameter `P`.

With those observations in mind, let's see what the compiler and the anlyzer
report.

Compile and analyze:
```
$ make -C functor
make: Entering directory '/tmp/docs/exported_values/code_constructs/functor'
ocamlopt -w +32 -bin-annot functor_lib.mli functor_lib.ml functor_bin.ml
File "functor_lib.ml", line 6, characters 2-25:
6 |   val unused_required : t
      ^^^^^^^^^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_required.

File "functor_lib.ml", line 11, characters 6-23:
11 |   let unused_unexported = P.used_required
           ^^^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_unexported.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:9: F.internally_used
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:10: F.unused
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:14: InternalParam.used_required
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:15: InternalParam.unused_required
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:25: InternalApp.internally_used
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:26: InternalApp.unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/functor'
```

The compiler tells us that `unused_required` is unused in the implementation of
`F`. It also reports `unexported_unused` in `F` like it did in the
[module example](#module) for regular modules. These 2 values can be removed at
the indicated locations.

The analyzer reports 6 unused exported values, all in the `functor_lib.mli` :
2 in `F`, 2 in `InternalParam`, and 2 in `InternalApp`. Let's observe them by
module (in reverse order) :

- The reports for `InternalApp` are identical to the [module example](#module).
Although, `InternalApp` is the result of applying `F`, it has an explicit
signature. I.e. it exposes its own values and the link between them and those of
`F` is absent from the signature. Consequently, they are tracked independently.

- As we observed before runnning the analyzer, the values in `InternalParam` are
used by requirement. However, this use is internal to `Functor_lib`, and there
is an interface available : `functor_lib.mli`. Consequently, the internal uses
are ignored, and `InternalParam`'s values become unused.

> [!NOTE]
> The kind of use (explicit reference, or by requirement) has no influence on
> the tracking mode (external only, or internal + external).

- `F` is a functor but is tracked like a regular module. The reported values are
those of its result module. Reporting on those may feel like duplicates, but,
as explained for the reports of `InternalApp`, the values of the result
module are declared independently of those of `InternalApp`, hence they are
tracked and reported independently.

All the values reported by the analyzer can be safely removed.

Before moving on, there is another observation that we can make :
the values `unused` and `internally_used` of `ExternalApp` are not reported.
Because they are reported for `InternalApp` and `F`, it would be natural to
expect them in the reports for `ExternalApp` as well. In reality, they are not
tracked individually for `ExternalApp` because it does not expose them
explicitly. Unlike `InternalApp` which has an explicit module signature,
`ExternalApp` does not. Consequently, its values are directly linked to those of
`F`. This situation will be explored in the
[module signature](#module-signature) example.

> [!TIP]
> If we activated the compiler warning 67 `unused-functor-parameter`(by
> passing the argument `-w +67`), then the compiler would have reported :
> ```
> File "functor_lib.mli", line 4, characters 10-11:
> 4 | module F (P : sig
>               ^
> Warning 67 [unused-functor-parameter]: unused functor parameter P.
> ```
> This can be fixed by either replacing `P` with `_`, or by rewriting the
> declaration of `F` as :
> ```OCaml
> module F : sig
>     val used_required : t
>     val unused_required : t
>   end
>   -> sig
>     val externally_used : t
>     val internally_used : t
>     val unused : t
>   end
> ```

#### Removing the unused values

In addition to removing everything that was reported by the compiler and the
analyzer, we also commented out `P.used_required` and `InternalParam` in
`functor_lib.mli`. It is up to the user to decide whether they would like to
keep them or remove them from their specifications. Neither would be reported
by the compiler or the analyzer.

> [!NOTE]
> Functors are covariant in their parameters. I.e. it is allowed to give a
> larger module as argument than what the parameter specifies.
> Similarly it is allowed to declare the parameter larger in the interface than
> it is in the implementation. Consequently, the compiler would not complain if
> `P` would expect `unused_required` in the `.mli` but not in the `.ml`.

Code:
```OCaml
(* functor_lib.mli *)
type t

module F (P : sig
  val used_required : t
  (* val unused_required : t *)
end) : sig
  val externally_used : t
end

(*
module InternalParam : sig
end
*)

module ExternalParam : sig
  val used_required : t
  val unused_required : t
end

module InternalApp : sig
  val externally_used : t
end
```
```OCaml
(* functor_lib.ml *)
type t = unit

module F (P : sig
  val used_required : t
end) = struct
  let externally_used = P.used_required
  let internally_used = P.used_required
  let unused = P.used_required
  let () = internally_used
end

module InternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module ExternalParam = struct
  let used_required = ()
  let unused_required = ()
end

module InternalApp = F(InternalParam)
```
```OCaml
(* functor_bin.ml *)
open Functor_lib

module ExternalApp = F(ExternalParam)

let () =
  ignore InternalApp.externally_used;
  ignore ExternalApp.externally_used
```

Compile and analyze :
```
$ make -C functor
make: Entering directory '/tmp/docs/exported_values/code_constructs/functor'
ocamlopt -w +32 -bin-annot functor_lib.mli functor_lib.ml functor_bin.ml
File "functor_lib.ml", line 9, characters 6-12:
9 |   let unused = P.used_required
          ^^^^^^
Warning 32 [unused-value-declaration]: unused value unused.

File "functor_lib.ml", line 15, characters 6-21:
15 |   let unused_required = ()
           ^^^^^^^^^^^^^^^
Warning 32 [unused-value-declaration]: unused value unused_required.
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/code_constructs/functor/functor_lib.mli:18: ExternalParam.unused_required

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/code_constructs/functor'
```

Now that `F.unused` and `InternalParam.unused_required` are not exported, they
are reported as unused by the compiler, and can be removed safely.

`ExternalParam.unused_required` was used by requirement. Now that it is not
required by `P` (because it is commented out), it is unused and the analyzer
correctly reports it. It can be removed safely. Removing it will trigger the
same compiler warning as for `InternalParam.unused_required`, so it can be
removed from both the interface and the implementation.

The unused values can be removed as explained. Our work here is done.
