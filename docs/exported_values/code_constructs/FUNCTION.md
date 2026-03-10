The reference files for this example are in the
[function](../../../examples/docs/exported_values/code_constructs/function) directory.

The reference takes place in `/tmp/docs/exported_values/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/exported_values/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

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

## First run

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
arguments to a function or applying them (even partially) count as uses just
like any other explicit reference. Therefore, `Function_lib`'s `memoize`,
`heavy_computation`, and `do_nothing` are used in `Function_bin`. This leaves
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

As expected, the analyzer only reports `unused`, declared in `function_lib.mli`.

## Removing the unused values

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

Now that `unused` is unexported, the compiler reports it as unused via the
warning 32, and the analyzer does not report anything. Removing that value
fixes all the warnings. Our work here is done.
