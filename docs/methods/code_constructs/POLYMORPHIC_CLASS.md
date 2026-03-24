The reference files for this example are in the
[polymorphic\_class](../../../examples/docs/methods/code_constructs/polymorphic_class) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C polymorphic_class build
```

The analysis command is :
```
make -C polymorphic_class analyze
```

The compile + analyze command is :
```
make -C polymorphic_class
```

## First run

Code:
```OCaml
(* polymorphic_class_lib.mli *)
class ['a] stack :
  object
    method push : 'a -> unit
    method pop : unit
    method peek : 'a option
    method reset : unit
  end
```
```OCaml
(* polymorphic_class_lib.ml *)
class ['a] stack =
  object
    val mutable l : 'a list = []
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
(* polymorphic_class_bin.ml *)
let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let int_stack = new Polymorphic_class_lib.stack in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
```

Before looking at the analysis results, let's look at the code.

This example is very similar to the [Class](./CLASS.md) example, without the
`unused_class` class. Instead of using an definitive `int_stack` class, the
`Polymorphic_class_lib` defines a polymorphic `['a] stack` class.

Methods `push`, `peek`, and `pop` are used, leaving `reset` as the only unused method.

Compile and analyze:
```
$ make -C polymorphic_class/
make: Entering directory '/tmp/docs/methods/code_constructs/polymorphic_class'
ocamlopt -bin-annot polymorphic_class_lib.mli polymorphic_class_lib.ml polymorphic_class_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/polymorphic_class/polymorphic_class_lib.mli:2: stack#reset

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/polymorphic_class'
```

As expected, the analyzer reports `stack#reset` as unused.

> [!NOTE]
> The analyzer does not specify the type parameter of `stack`. This is because
> it does not distinguish the monomorphizations of `stack` and its polymoprhic
> definition.

After removing the reported unused methods, the analyzer will not find anymore
unused method. Our work here is done.
