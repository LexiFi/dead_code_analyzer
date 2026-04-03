# Table of contents

+ [Methods](#methods)
    + [Definitions](#definitions)
    + [Compiler warnings](#compiler-warnings)
        + [Warning 26: unused-ancestor](#warning-36-unused-ancestor)
    + [Usage](#usage)
+ [Examples](#examples)
+ [Limitations](#limitations)
    +[Class type](#class-type)
    +[Object type](#object-type)
    +[Alias](#alias)
    +[Factory function](#factory-function)

# Methods

## Definitions

A **method** is declared using the `method` keyword in the definition of a class
or an immediate object, or as a field of an object type.

An **instance variable** is declared using the `val` keyword in the definition
of a class or an immediate object. It does not appear in object types.

A **public** method is not a private method.
A **private** method is one declared with the `private` keyword.

An **exported** method is one that exists in its compilation unit's signature

A **use** is either :
- An explicit reference.
  E.g.
  ```OCaml
  let o = object method answer = 42 end
  let () = print_int o#answer
  ```
  The method `answer` is explicitly referenced in `o#answer`.
- A requirement for that method to exist.
  E.g.
  ```OCaml
  let print_answer param = print_int param#answer
  let o = object method answer = 42 end
  let () = print_answer o
  ```
  The type of `print_answer` is `< answer : int; .. > -> unit`, meaning that its
  arguments are required to at least provide a method named `answer`. Therefore,
  `o#answer` is used by requirement in `print_answer o`. If `o` did not provide
  method `answer`, then the compilation would fail with an error like :
  ```
  File "requirement.ml", line 4, characters 22-23:
  4 | let () = print_answer o
                            ^
  Error: This expression has type <  > but an expression was expected of type
           < answer : int; .. >
         The first object type has no method answer
  ```

## Compiler warnings

The analyzer reports unused exported public methods. The compiler does not
report unused methods (private or public, exported or not, from a class, an
immediate, or an object type), not instance vraiables. Thus, the 2 tools do not
complement each other, and unused unexported methods or unused exported private
methods remain undetected.

> [!WARNING]
> Only a portion of the unused object-related code can be reported using the
> compiler and the analyzer : unused exported public methods. Unused private or
> unexported methods, as well as unused instance variables will remain
> undetected.

> [!IMPORTANT]
> Exported values of object or class types belong to the
> [Exported values](../exported_values/EXPORTED_VALUES.md) section.

Although the compiler does not report unused methods or instance variables, it
still has a few object-related warnings, of which one is related to unused code
constructs : warning 36.

### Warning 36: unused-ancestor

This warning is disabled by default.
It can be enabled by passing `-w +36` to the compiler.

Description:
```
36 [unused-ancestor] Unused ancestor variable. (since 4.00)
```

Example:
```OCaml
(* warning36.ml *)
class c1 = object end
class c2 = object
   inherit c1 as super
end
```
```
$ ocamlopt -w +36 warning36.ml
File "warning36.ml", line 4, characters 3-22:
4 |    inherit c1 as super
       ^^^^^^^^^^^^^^^^^^^
Warning 36 [unused-ancestor]: unused ancestor variable super.
```

## Usage

Unused exported public methods are reported by default.
Their reports can be deactivated by using the `--nothing` or `-M nothing`
command line arguments.
They can be reactivated by using the `--all` or `-M all` command line arguments.
For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

The report section looks like:

```
.> UNUSED METHODS:
=================
filepath:line: source#method

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is `filepath:line: value` with `filepath` the absolute
path to the file (`.mli` if available, `.ml` otherwise) where `source` is
declared, `line` the line index in `filepath` at which `source` is declared,
`source` the path of the object/class within its compilation unit (e.g. `M.c`)
which declares `method`, and `method` the unused method.
There can be any number of such lines.

The expected resolution for an unused exported public method is to remove it
from the `.mli` if there is one and the `.ml`.

> [!IMPORTANT]
> Removing unused methods from the codebase may trigger the detection of new
> unused methods and new unused values. Removing unused values may trigger the
> detection of new unused methods or remove some in the case of values of
> object types.
> Consequently, it is expected that a user might need to compile and analyze
> their code multiple times when cleaning up their codebase.

# Examples

- The [code constructs](./code_constructs) directory contains a collection of
  examples dedicated to specific code constructs :
    - [Class](./code_constructs/CLASS.md)
    - [Polymorphic class](./code_constructs/POLYMORPHIC_CLASS.md)
    - [Constructor](./code_constructs/CONSTRUCTOR.md)
    - [Class type](./code_constructs/CLASS_TYPE.md)
    - [Inheritance](./code_constructs/INHERITANCE.md)
    - [Immediate object](./code_constructs/IMMEDIATE_OBJECT.md)
    - [Factory function](./code_constructs/FACTORY_FUN.md)
    - [Object type](./code_constructs/OBJECT_TYPE.md)
    - [Coercion](./code_constructs/COERCION.md)

# Limitations

## Class type

As explained in the [Class type](./code_constructs/CLASS_TYPE.md) example, the
analyzer is currently restricted to not reporting methods declared in class
type definitions.

## Object type

As explained in the [Object type](./code_constructs/OBJECT_TYPE.md) example, the
analyzer is currently restricted to not reporting methods declared in object
types.

## Alias

Related issue :
[issue #66](https://github.com/LexiFi/dead_code_analyzer/issues/66).

In the presence of multiple bindings to the same object, the analyzer corrctly
avoids tracking their methods individually. However, it fails at unifying them
and only keeps track of the methods used through the original binding, where the
methods are defined. This leads to **false positives**.

### Example

The reference files for this example are in the
[alias](../../examples/docs/methods/limitations/alias) directory.

The reference takes place in `/tmp/docs/methods/limitations`, which
is a copy of the [limitations](../../../examples/docs/methods/limitations)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C alias build
```

The analysis command is :
```
make -C alias analyze
```

The compile + analyze command is :
```
make -C alias
```

Code:
```OCaml
(* alias_lib.mli *)
val original :
  < used : unit
  ; used_by_alias : unit
  ; unused : unit
  >

val alias :
  < used : unit
  ; used_by_alias : unit
  ; unused : unit
  >
```
```OCaml
(* alias_lib.ml *)
let original =
  object
    method used = ()
    method used_by_alias = ()
    method unused = ()
  end

let alias = original
```
```OCaml
(* alias_bin.ml *)
open Alias_lib

let () =
  original#used;
  alias#used_by_alias
```

Compile and analyze:
```
$ make -C alias
make: Entering directory '/tmp/docs/methods/limitations/alias'
ocamlopt -bin-annot alias_lib.mli alias_lib.ml alias_bin.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/limitations/alias/alias_lib.mli:2: original#unused
/tmp/docs/methods/limitations/alias/alias_lib.mli:2: original#used_by_alias

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/limitations/alias'
```

The analyzer reports `original#used_by_alias` although it is used by
`alias#used_by_alias`.

## Factory function

Related issue :
[issue #67](https://github.com/LexiFi/dead_code_analyzer/issues/67).

Factory functions' methods analysis is currently very limited to situations like
the one in the [Factory function](./code_constructs/FACTORY_FUN.md) example :
functions without intermediate binding to the returned object in at least one
branch. I.e. if in all the branches, the result object is bound to a name, then
the analyzer fails to track its methods. This leads to **false negatives**.

### Example

The reference files for this example are in the
[factory\_fun\_indir](../../examples/docs/methods/limitations/factory_fun_indir) directory.

The reference takes place in `/tmp/docs/methods/limitations`, which
is a copy of the [limitations](../../../examples/docs/methods/limitations)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C factory_fun_indir build
```

The analysis command is :
```
make -C factory_fun_indir analyze
```

The compile + analyze command is :
```
make -C factory_fun_indir
```

Code:
```OCaml
(* factoy_fun_indir.ml *)
let factory_with_intermediate_binding () =
  let res =
    object method m = () end
  in
  res

let random_factory () =
  if Random.bool () then
    object method m = () end
  else
    let res = object method m = () end in
    res
```

Compile and analyze:
```
$ make -C factory_fun_indir
make: Entering directory '/tmp/docs/methods/limitations/factory_fun_indir'
ocamlopt -bin-annot factory_fun_indir.ml
dead_code_analyzer --nothing -M all .
Scanning files...
 [DONE]

.> UNUSED METHODS:
=================
/tmp/docs/methods/limitations/factory_fun_indir/factory_fun_indir.ml:8: random_factory#m

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/methods/limitations/factory_fun_indir'
```

The analyzer correctly reports `random_factory#m` because its last expression in
the `if` branch is the object's definition. It does not report
`factory_with_intermediate_binding#m` because the returned object is bound
inside the function.

> [!NOTE]
> The analyzer does not distinguish which object is actually returned when there
> are alternatives like in `random_factory`. Only uses outside of the function
> are accounted for. E.g. using `res#m` in the `else` branch does not count.
