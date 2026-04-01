The reference files for this example are in the
[factory_fun](../../../examples/docs/methods/code_constructs/factory_fun) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C factory_fun build
```

The analysis command is :
```
make -C factory_fun analyze
```

The compile + analyze command is :
```
make -C factory_fun
```

> [!IMPORTANT]
> **LIMITATION**
>
> Only factory functions that return the object without intermediate binding
> in at least one branch are understood by the analyzer. This may lead to
> **false negatives**.
> See [Factory function | Limitations](../METHODS.md#factory-function).
>
> Storing the result of a factory function in a new binding breaks the analysis.
> Although 2 calls to the same factory function may produce different objects,
> they are considered to be the same by the analyzer. Hence, storing the result
> in a new binding is similar to an alias, which may lead to **false positives**.
> See [Alias | Limitations](../METHODS.md#alias)

## First run

Code:
```OCaml
(* factoy_fun_lib.mli *)
val get_stack :
  unit ->
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >
```
```OCaml
(* factoy_fun_lib.ml *)
let stack = ref []

let get_stack () =
  object
    method push x = stack := x::!stack
    method pop =
      match !stack with
      | [] -> ()
      | _::tl -> stack := tl
    method peek =
      match !stack with
      | [] -> None
      | hd::_ -> Some hd
    method reset = stack := []
  end
```
```OCaml
(* factoy_fun_bin.ml *)
let unused_factory () = object method unused_method = () end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let open Factory_fun_lib in
  let n = 42 in
  push_n_times n (get_stack ());
  while (get_stack ())#peek <> None do
    (get_stack ())#pop;
  done
```

Before looking at the analysis results, let's look at the code.

The `Factory_fun_lib` declares and exports 1 factory function `get_stack` which
takes `unit` as parameter and returns a fresh object. The returned object has
4 methods, manipulating the unexported value `stack`.
1 of these methods is used by requirement : `push`.
2 of these methods are explicitly referenced : `peek`, and `pop`.
This leaves `reset` as unused.

The `Factory_fun_bin` declares and exports 1 factory function `unused_factory`
which takes `unit` as parameter and returns a fresh object with one method
`unused_method`. This function is unused, leaving its method unused too.

Compile and analyze:
```
$ make -C factory_fun
make: Entering directory '/tmp/docs/methods/code_constructs/factory_fun'
ocamlopt -bin-annot factory_fun_lib.mli factory_fun_lib.ml factory_fun_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/factory_fun/factory_fun_bin.ml:2: unused_factory#unused_method
/tmp/docs/methods/code_constructs/factory_fun/factory_fun_lib.mli:2: get_stack#reset

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/factory_fun'
```

As expected, the analyzer reports `unused_factory#unused_method` and
`get_stack#reset` as unused. Following the report line format
`filepath:line: source#method`, here the sources are the functions themselves.
Indeed, the returned objects are defined as the immediate results of the
functions, making the functions behave similarly to class constructors.

The reported methods can be removed from the `.mli` and `.ml`.
Our work here is done.
