The reference files for this example are in the
[inheritance](../../../examples/docs/methods/code_constructs/inheritance) directory.

The reference takes place in `/tmp/docs/methods/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/methods/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C inheritance build
```

The analysis command is :
```
make -C inheritance analyze
```

The compile + analyze command is :
```
make -C inheritance
```

## First run

Code:
```OCaml
(* inheritance_lib.mli *)
class parent :
  object
    method unused : unit
    method used : unit
    method used_by_child : unit
  end

class child :
  object
    inherit parent
  end
```
```OCaml
(* inheritance_lib.ml *)
class parent =
  object
    method unused = ()
    method used = ()
    method used_by_child = ()
  end

class child =
  object
    inherit parent
  end
```
```OCaml
(* inheritance_bin.ml *)
let () =
  let p = new Inheritance_lib.parent in
  let c = new Inheritance_lib.child in
  p#used;
  c#used_by_child
```

The code is pretty straightforward. `Inheritance_lib` defines 2 classes :
- `parent`, which defines 3 methods;
- `child`, which only inherits `parent`.

CLass inheritance is semantically equivalent to module inclusion from the
analyzer's point of view : the `parent` class is the actual "owner" of the
methods, and `child` only re-exposes them. This, has 2 consequences :
- inherited methods in `child` are not analyzed individually;
- using a method from `child` that is inherited from `parent` is the same as
  using the method from `parent`.

Now if we look at the uses, 2 methods are referenced explicitly :
- `p#used`, which is `parent#used`
- `c#used_by_child`, which is `child#used_by_child`, which is
  `parent#used_by_child`

This leaves `parent#unused` as the only unused method.

Compile and analyze:
```
$ make -C inheritance
make: Entering directory '/tmp/docs/methods/code_constructs/inheritance'
ocamlopt -bin-annot inheritance_lib.mli inheritance_lib.ml inheritance_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/code_constructs/inheritance/inheritance_lib.mli:2: parent#unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/code_constructs/inheritance'
```

As expected, the analyzer reports `parent#unused` as unused. It can be removed
from `inheritance_lib.mli` and `inheritance_lib.ml`.
Neither the compiler nor the analyzer will report unused methods anymore.
Our work here is done.
