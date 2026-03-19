The reference files for this example are in the
[immediate\_object](../../../examples/docs/methods/code_constructs/immediate_object) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C immediate_object build
```

The analysis command is :
```
make -C immediate_object analyze
```

The compile + analyze command is :
```
make -C immediate_object
```

## First run

Code:
```OCaml
(* immediate_object_lib.mli *)
val int_stack :
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >
```
```OCaml
(* immediate_object_lib.ml *)
let int_stack =
  object
    val mutable l = []
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
(* immediate_object_bin.ml *)
let unused_obj = object method unused_method = () end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let open Immediate_object_lib in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
```

Before looking at the analysis results, let's look at the code.

The `Imm_obj_lib` declares and exports 1 object : `int_stack`, which is used by
`Imm_obj_bin`. `Imm_obj_bin` declares and exports 1 object : `unused_obj`, which
is unused. All of the methods are public, so they are tracked by the analyzer.

2 methods are explicitly referenced : `int_stack#peek`, and `int_stack#pop`.
1 method is used by requirement : `int_stack#push`, required by the call
`push_n_times n int_stack`.

This leaves only 2 unused methods : `int_stack#reset` and
`unused_obj#unused_method`.

Compile and analyze:
```
$ make -C immediate_object
make: Entering directory '/tmp/docs/methods/code_constructs/immediate_object'
ocamlopt -bin-annot immediate_object_lib.mli immediate_object_lib.ml immediate_object_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/immediate_object/immediate_object_bin.ml:2: unused_obj#unused_method
/tmp/docs/methods/code_constructs/immediate_object/immediate_object_lib.mli:2: int_stack#reset

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/immediate_object'
```

As expected, 2 methods are reported unused : `unused_obj#unused_method`, and
`int_stack#reset`. These can be removed from the object definitions at the
reported locations.

> [!IMPORTANT]
> The reported locations are those of the objects to which the methods belong.

In addition to these unused methods, there is the unused exported value
`unused_obj` which is not reported because we disabled the analysis of unused
values when passing `--nothing` to the analyzer. Let's update the Makefile and
pass `-E all` to the analyzer to enable that analysis.

```
SRC:=immediate_object_lib.mli immediate_object_lib.ml immediate_object_bin.ml

all: build analyze

build:
	ocamlopt -bin-annot ${SRC}

analyze:
	dead_code_analyzer --nothing -E all -M all .

clean:
	rm -f *.cm* *.o a.out
```

## Removing the unused methods

> [!TIP]
> Do not forget to remove `int_stack#reset` from both the `.mli` **and** the `.ml`.
> Otherwise, the compiler will reject the code with a message like :
> ```
> File "immediate_object_lib.ml", line 1:
> Error: The implementation immediate_object_lib.ml
>        does not match the interface immediate_object_lib.mli:
>        Values do not match:
>          val int_stack :
>            < peek : '_weak1 option; pop : unit; push : '_weak1 -> unit;
>              reset : unit >
>        is not included in
>          val int_stack :
>            < peek : int option; pop : unit; push : int -> unit >
>        The type
>          < peek : '_weak1 option; pop : unit; push : '_weak1 -> unit;
>            reset : unit >
>        is not compatible with the type
>          < peek : int option; pop : unit; push : int -> unit >
>        The second object type has no method reset
>        File "immediate_object_lib.mli", lines 2-6, characters 0-3:
>          Expected declaration
>        File "immediate_object_lib.ml", line 2, characters 4-13:
>          Actual declaration
> ```

Code:
```OCaml
(* immediate_object_lib.mli *)
val int_stack :
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  >
```
```OCaml
(* immediate_object_lib.ml *)
let int_stack =
  object
    val mutable l = []
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
(* immediate_object_bin.ml *)
let unused_obj = object end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let open Immediate_object_lib in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
```

Compile and analyze:
```
$ make -C immediate_object
make: Entering directory '/tmp/docs/methods/code_constructs/immediate_object'
ocamlopt -bin-annot immediate_object_lib.mli immediate_object_lib.ml immediate_object_bin.ml
dead_code_analyzer --nothing -E all -M all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/methods/code_constructs/immediate_object/immediate_object_bin.ml:2: unused_obj

Nothing else to report in this section
--------------------------------------------------------------------------------


.> UNUSED METHODS:
=================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/immediate_object'
```

There is no more unused method. The unused exported value can be removed as
explained in the [Exported values](../../exported_values/EXPORTED_VALUES.md) documentation.
Our work here is done
