The reference files for this example are in the
[class](../../../examples/docs/methods/code_constructs/class) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C class build
```

The analysis command is :
```
make -C class analyze
```

The compile + analyze command is :
```
make -C class
```

## First run

Code:
```OCaml
(* class_lib.mli *)
class int_stack :
  object
    method push : int -> unit
    method pop : unit
    method peek : int option
    method reset : unit
  end
```
```OCaml
(* class_lib.ml *)
class int_stack =
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
(* class_bin.ml *)
class unused_class = object method unused_method = () end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let int_stack = new Class_lib.int_stack in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
```

Before looking at the analysis results, let's look at the code.

The `Class_lib` declares and exports 1 class : `int_stack`. An instance of this
class is used in `Class_bin`.
`Class_bin` declares and exports 1 class : `unused_class`, which is unused.
All of the methods are public, so they are tracked by the analyzer.

2 methods are explicitly referenced : `int_stack#peek`, and `int_stack#pop`.
1 method is used by requirement : `int_stack#push`, required by the call
`push_n_times n int_stack`.

This leaves only 2 unused methods : `int_stack#reset` and
`unused_obj#unused_method`.

Compile and analyze:
```
$ make -C class
make: Entering directory '/tmp/docs/methods/code_constructs/class'
ocamlopt -bin-annot class_lib.mli class_lib.ml class_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/class/class_bin.ml:2: unused_class#unused_method
/tmp/docs/methods/code_constructs/class/class_lib.mli:2: int_stack#reset

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/class'
```

As expected, 2 methods are reported unused : `unused_class#unused_method`, and
`int_stack#reset`. These can be removed from the class definitions at the
reported locations.

> [!IMPORTANT]
> The reported locations are those of the classes to which the methods belong.

> [!NOTE]
> The objects manipulated in this example are instances of classes. Thus, their
> methods are those of their classes.
> If the analyzer reported unused methods for each instance, a user would need
> to explicit the signature of each object rather than simply rely on the
> (possibly inferred) defined class type. This is a non-trivial resolution
> of unused methods, effectively losing the benefits of using class types.
> Consequently, the analyzer does not report unused methods of instances but
> only of the classes they belong to.

In addition to these unused methods, there is the unused exported class
`unused_class` which is currently not reportable by the analyzer.

## Removing the unused methods

> [!TIP]
> Do not forget to remove `int_stack#reset` from both the `.mli` **and** the `.ml`.
> Otherwise, the compiler will reject the code with a message like :
> ```
> File "class_lib.ml", line 1:
> Error: The implementation class_lib.ml
>        does not match the interface class_lib.mli:
>        Class declarations do not match:
>          class int_stack :
>            object
>              val mutable l : int list
>              method peek : int option
>              method pop : unit
>              method push : int -> unit
>              method reset : unit
>            end
>        does not match
>          class int_stack :
>            object
>              method peek : int option
>              method pop : unit
>              method push : int -> unit
>            end
>        The public method reset cannot be hidden
> ```

Code:
```OCaml
(* class_lib.mli *)
class int_stack :
  object
    method push : int -> unit
    method pop : unit
    method peek : int option
  end
```
```OCaml
(* class_lib.ml *)
class int_stack =
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
  end
```
```OCaml
(* class_bin.ml *)
class unused_class = object end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let int_stack = new Class_lib.int_stack in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
```

Compile and analyze:
```
$ make -C class
make: Entering directory '/tmp/docs/methods/code_constructs/class'
ocamlopt -bin-annot class_lib.mli class_lib.ml class_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/class'
```

There is no more unused method. Our work here is done.
