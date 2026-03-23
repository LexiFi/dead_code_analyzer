The reference files for this example are in the
[constructor](../../../examples/docs/methods/code_constructs/constructor) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C constructor build
```

The analysis command is :
```
make -C constructor analyze
```

The compile + analyze command is :
```
make -C constructor
```

## First run

Code:
```OCaml
(* constructor_lib.mli *)
class int_stack :
  int list ->
  object
    method push : int -> unit
    method pop : unit
    method peek : int option
    method reset : unit
  end
```
```OCaml
(* constructor_lib.ml *)
class int_stack init =
  object
    val mutable l : int list = init
    method push x = l <- x::l
    method pop =
      match l with
      | [] -> ()
      | _::tl -> l <- tl
    method peek =
      match l with
      | [] -> None
      | hd::_ -> Some hd
    method reset = l <- []
  end
```
```OCaml
(* constructor_bin.ml *)
class unused_class _ = object method unused_method = () end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let int_stack = new Constructor_lib.int_stack [] in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
```

The code is very similar to the one of the [Class](./CLASS.md) example, with a
small twist : the classes have initialisers. Because the analyzer does not track
each instance' methods uses individually but unified whith the defining class,
the presence of initialisers should not make a difference and the expected
results are the same as in the [Class](./CLASS.md) example.

Compile and analyze:
```
$ make -C constructor
make: Entering directory '/tmp/docs/methods/code_constructs/constructor'
ocamlopt -bin-annot constructor_lib.mli constructor_lib.ml constructor_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/constructor/constructor_bin.ml:2: unused_class#unused_method
/tmp/docs/methods/code_constructs/constructor/constructor_lib.mli:2: int_stack#reset

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/constructor'
```

The results are the same as expected. The resolution is already detailed in the
[Class](./CLASS.md) example. Our work here is done.
