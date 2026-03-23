The reference files for this example are in the
[object\_type](../../../examples/docs/methods/code_constructs/object_type) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C object_type build
```

The analysis command is :
```
make -C object_type analyze
```

The compile + analyze command is :
```
make -C object_type
```

> [!IMPORTANT]
> **LIMITATION**
>
> Methods declared in object types are currently ignored by the analyzer.

## First run

Code:
```OCaml
(* object_type_lib.mli *)
type int_stack =
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >

val int_stack : int_stack
```
```OCaml
(* object_type_lib.ml *)
type int_stack =
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >

let int_stack =
  object
    val mutable l : int list = []
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
(* object_type_bin.ml *)
type unused_t = < unused_method : unit >

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let open Object_type_lib in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
```

By looking at the code, we could make the same observation as in the
[Class type](./CLASS_TYPE.md) example. The analyzer's semantics are equivalent
so the results are the same as well.

Code and analyze:
```
$ make -C object_type
make: Entering directory '/tmp/docs/methods/code_construct/object_type'
ocamlopt -bin-annot object_type_lib.mli object_type_lib.ml object_type_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_construct/object_type'
```

Nothing is reported. Our work here is done.
