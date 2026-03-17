# Table of contents

+ [Exported Values](#exported-values)
    + [Definitions](#definitions)
    + [Compiler warnings](#compiler-warnings)
        + [Warning 26: unused-var](#warning-26-unused-var)
        + [Warning 27: unused-var-strict](#warning-27-unused-var-strict)
        + [Warning 32: unused-value-declaration](#warning-32-unused-value-declaration)
    + [Usage](#usage)
+ [Examples](#examples)
+ [Limitations](#limitations)
    + [Module type](#module-type)
    + [Include module type with substitution](#include-module-type-with-substitution)
    + [Including a module with same name](#including-a-module-with-the-same-name)

# Exported Values

## Definitions

A **value** is the result of an expression. For the analyzer, we'll restrict
this definition to a _named_ value, i.e. a name bound to an expression.
In general, this is the `lhs` of `let lhs = ...` or `val lhs : ...`.

An **exported** value is one that is accessible outside its compilation unit.
I.e. a value that can be referenced in other `.ml` and/or `.mli` files than
the ones that declare it.

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

## Compiler warnings

The analyzer reports unused _exported_ values while the compiler reports other
kinds of unused values. They complement each other.

> [!TIP]
> To obtain a list of available compiler warnings, use
> `ocamlopt -warn-help`

The compiler warnings related to unused values are the 26, 27, and 32.
The two firsts warn about unused local values. The third warns about unused
unexported toplevel values.

### Warning 26: unused-var

This warning is enabled by default.
I can be disabled by passing the `-w -26` to the compiler.

Description:
```
26 [unused-var] Suspicious unused variable: unused variable that is bound
    with "let" or "as", and doesn't start with an underscore ("_")
    character.
```

Example:
```OCaml
(* warning26.ml *)
let () =
  let x = () in
  ()
```
```
$ ocamlopt warning26.ml
File "warning26.ml", line 3, characters 6-7:
3 |   let x = () in
          ^
Warning 26 [unused-var]: unused variable x.
```

### Warning 27: unused-var-strict

This warning is disabled by default.
I can be enabled by passing the `-w +27` to the compiler.

Description:
```
27 [unused-var-strict] Innocuous unused variable: unused variable that is not bound with
    "let" nor "as", and doesn't start with an underscore ("_")
    character.
```

Example:
```OCaml
(* warning27.ml *)
let f = function
  | x -> ()
```
```
$ ocamlopt -w +27 warning27.ml
File "warning27.ml", line 2, characters 4-5:
2 |   | x -> ()
        ^
Warning 27 [unused-var-strict]: unused variable x.
```

### Warning 32: unused-value-declaration

This warning is disabled by default.
I can be enabled by passing the `-w +32` to the compiler.

Description:
```
32 [unused-value-declaration] Unused value declaration. (since 4.00)
```

Example:
```OCaml
(* warning32.mli *)
```
```OCaml
(* warning32.ml *)
let x = ()
```
```
$ ocamlopt -w +32 warning32.mli warning32.ml
File "warning32.ml", line 2, characters 4-5:
2 | let x = ()
        ^
Warning 32 [unused-value-declaration]: unused value x.
```

## Usage

Unused exported values are reported by default.
Their reports can be deactivated by using the `--nothing` or `-E nothing`
command line arguments.
They can be reactivated by using the `--all` or `-E all` command line arguments.
For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

The report section looks like:
```
.> UNUSED EXPORTED VALUES:
=========================
filepath:line: value

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is `filepath:line: value` with `filepath` the absolute
path to the file (`.mli` if available, `.ml` otherwise) where `value` is
declared, `line` the line index in `filepath` at which `value` is declared, and
`value` the path of the value within its compilation unit (e.g. `M.x`).
There can be any number of such lines.

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

E.g.
```OCaml
(* not_transitive.ml *)
let () =
  let x = () in
  let y = x in
  ()
```
```
$ ocamlopt not_transitive.ml
File "not_transitive.ml", line 4, characters 6-7:
4 |   let y = x in
          ^
Warning 26 [unused-var]: unused variable y.
```
`y` is reported by the warning 26 but not `x`, because `x` is used by `y`.

# Examples

- [Hello world](./HELLO_WORLD.md) is a collection of small examples relying on the
  same code, but organized differently (with/without `.mli`, single/multiple
  compilation units). This aims at providing a first walk-through of the
  analyzer's usage.

- The [code constructs](./code_constructs) directory contains a collection of
  examples dedicated to specific code constructs :
    - [Function](./code_constructs/FUNCTION.md)
    - [Module](./code_constructs/MODULE.md)
    - [Functor](./code_constructs/FUNCTOR.md)
    - [Module type](./code_constructs/MODTYP.md)
    - [Module signature](./code_constructs/MODSIG.md)
    - [Include](./code_constructs/INCLUDE.md)

# Limitations

## Module type

Related issue :
[issue #50](https://github.com/LexiFi/dead_code_analyzer/issues/50).

As explained in the [Module type](./code_constructs/MODTYP.md) example, the
analyzer is currently restrcited to not reporting values declared in module
types. This means that any unused value defined by a module with a module type
as signature, even with constraints or substitutions, will not be reported.

A future improvement would be to report unused exported values declared in
module types by considering all the values defined in modules of such types as
instances of the values in the module types.

## Include module type with substitution

Related issue :
[issue #64](https://github.com/LexiFi/dead_code_analyzer/issues/64).

According to the above limitation on values in module types, values included
from a module type should not be reported. Even more so according to the
semantics described in the [Include](./code_constructs/INCLUDE.md) example.
This is the case unless there is a substitution on the module type.

### Example

The reference files for this example are in the
[sigincl](../examples/docs/exported_values/limitations/sigincl) directory.

The reference takes place in `/tmp/docs/exported_values/limitations`, which
is a copy of the [limitations](../examples/docs/exported_values/limitations)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C sigincl build
```

The analysis command is :
```
make -C sigincl analyze
```

The compile + analyze command is :
```
make -C sigincl
```

Code:
```OCaml
(* sigincl_lib.mli *)
module type T = sig
  type t
  val x : t
end

module M : T

module I : sig
  include T with type t := unit
end
```
```OCaml
(* sigincl_lib.ml *)
module type T = sig
  type t
  val x : t
end

module M = struct
  type t = unit
  let x = ()
end

module I = M
```

Compile and analyze:
```
$ make -C sigincl
make: Entering directory '/tmp/docs/exported_values/limitations/sigincl'
ocamlopt -bin-annot sigincl_lib.mli sigincl_lib.ml
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/limitations/sigincl/sigincl_lib.mli:10: I.x

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/limitations/sigincl'
```

The analyzer reports `I.x` at the line where `T` is included although it is
actually declared in `T`.

## Including a module with the same name

Related issue :
[issue #55](https://github.com/LexiFi/dead_code_analyzer/issues/55).

According to the semantics described in the
[Include](./code_constructs/INCLUDE.md) example, values re-exported via included
should not be reported. However, it may happen that values re-exported by
including a compilation unit out of the current codebase are reported when the
re-exporting compilation unit has the same name as the one included.

### Example

The reference files for this example are in the
[incl\_same\_name](../examples/docs/exported_values/limitations/incl_same_name)
directory.

The reference takes place in `/tmp/docs/exported_values/limitations`, which
is a copy of the [limitations](../examples/docs/exported_values/limitations)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C incl_same_name build
```

The analysis command is :
```
make -C incl_same_name analyze
```

The compile + analyze command is :
```
make -C incl_same_name
```

Code:
```OCaml
(* oo.ml *)
include Stdlib.Oo
```

Compile and analyze:
```
$ make -C incl_same_name
make: Entering directory '/tmp/docs/exported_values/limitations/incl_same_name'
ocamlopt -bin-annot oo.ml
dead_code_analyzer --nothing -E all .
Scanning files...
 [DONE]

.> UNUSED EXPORTED VALUES:
=========================
/tmp/docs/exported_values/limitations/incl_same_name/oo.mli:20: copy
/tmp/docs/exported_values/limitations/incl_same_name/oo.mli:25: id
/tmp/docs/exported_values/limitations/incl_same_name/oo.mli:40: new_method
/tmp/docs/exported_values/limitations/incl_same_name/oo.mli:41: public_method_label

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/exported_values/limitations/incl_same_name'
```

The analyzer reports values in `oo.mli` although they are included from
`Stdlib.Oo`.
