# Table of contents

+ [Fields and constructors](#fields-and-constructors)
    + [Definitions](#definitions)
    + [Compiler warnings](#compiler-warnings)
        + [Warning 37: unused-constructor](#warning-37-unused-constructor)
        + [Warning 38: unused-extension](#warning-38-unused-extension)
        + [Warning 69: unused-field](#warning-69-unused-field)
    + [Usage](#usage)
+ [Examples](#examples)

# Fields and constructors

## Definitions

A **constructor** is a named value of a variant type.

A **field** is a named value inside a record type.

An **exported** constructor or field is one that is is declared in its
compilation unit's signature.

A **private** field or constructor is one in a private record or variant type.
A private type is declared using the `private` keyword.

A **use** is either :
- Applying a constructor.
  E.g.
  ```OCaml
  type side = Left | Right

  let make_left () = Left
  ```
  The constructor `Left` is applied in `make_left`.
- Reading a field.
  E.g.
  ```OCaml
  type point = {x : float; y : float}

  let get_x p = p.x
  ```
  The field `point.x` is read in `get_x`.

> [!IMPORTANT]
> A **use** is **not**:
> - De-structuring a constructor.
>   E.g.
>   ```OCaml
>   type side = Left | Right
>
>   let string_of_side = function
>     | Left -> "Left"
>     | Right -> "Right"
>   ```
>   Constructors `Left` and `Right` are matched on but never applied.
> - Writing to a field.
>   E.g.
>   ```OCaml
>   type point = {x : float; y : float}
>
>   let make_point x y = {x; y}
>   ```
>   The fields `point.x` and `point.y` are written but never read.

## Compiler warnings

The analyzer reports unused exported fields and constructors, while the compiler
reports unused unexported fields and constructors. They complement each other.
The compiler also warns on unused private constructors (but not private fields
since they can still be read). The analyzer's reports overlap with the compiler
on unused exported private constructors.

> [!TIP]
> To obtain a list of available compiler warnings, use
> `ocamlopt -warn-help`

The compiler warnings related to unused fields and constructors are the 37, 38,
and 69.
The first one is for unexported constructors, the second for unexported fields.

### Warning 37: unused-constructor

This warning is disabled by default.
It can be enabled by passing the `-w +37` to the compiler.

Description:
```
37 [unused-constructor] Unused constructor. (since 4.00)
```

Example
```OCaml
(* warning37.mli *)
```
```OCaml
(* warning37.ml *)
type 'a opt = None | Some of 'a

let is_some = function
  | Some _ -> true
  | _ -> false
```
```
$ ocamlopt -w +37 warning37.mli warning37.ml
File "warning37.ml", line 2, characters 14-18:
2 | type 'a opt = None | Some of 'a
                  ^^^^
Warning 37 [unused-constructor]: unused constructor None.

File "warning37.ml", line 2, characters 19-31:
2 | type 'a opt = None | Some of 'a
                       ^^^^^^^^^^^^
Warning 37 [unused-constructor]: constructor Some is never used to build values.
(However, this constructor appears in patterns.)
```

### Warning 38: unused-extension

This warning is disabled by default.
It can be enabled by passing the `-w +38` to the compiler.

Description:
```
38 [unused-extension] Unused extension constructor. (since 4.00)
```

Example
```OCaml
(* warning38.mli *)
```
```OCaml
(* warning38.ml *)
type t = ..
type t +=
  | Int of int
  | Float of float

let is_int = function
  | Int _ -> true
  | _ -> false
```
```
$ ocamlopt -w +38 warning38.mli warning38.ml
File "warning38.ml", line 4, characters 2-14:
4 |   | Int of int
      ^^^^^^^^^^^^
Warning 38 [unused-extension]: extension constructor Int is never used to build values.
(However, this constructor appears in patterns.)

File "warning38.ml", line 5, characters 2-18:
5 |   | Float of float
      ^^^^^^^^^^^^^^^^
Warning 38 [unused-extension]: unused extension constructor Float
```

### Warning 69: unused-field

This warning is disabled by default.
It can be enabled by passing the `-w +69` to the compiler.

Description:
```
69 [unused-field] Unused record field. (since 4.13)
```

Example:
```OCaml
(* warning69.mli *)
```
```OCaml
(* warning69.ml *)
type point = {x : float; y : float}

let move_x p x = {p with x}
```
```
$ ocamlopt -w +69 warning69.mli warning69.ml
File "warning69.ml", line 2, characters 14-24:
2 | type point = {x : float; y : float}
                  ^^^^^^^^^^
Warning 69 [unused-field]: record field x is never read.
(However, this field is used to build or mutate values.)

File "warning69.ml", line 2, characters 25-34:
2 | type point = {x : float; y : float}
                             ^^^^^^^^^
Warning 69 [unused-field]: unused record field y.
```

## Usage

Unused exported fields and records are reported by default.
Their reports can be deactivated using the `--nothing` or `-T nothing`
command line arguments.
They can be reactivated by using the `--all` or `-T all` command line arguments.
For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

The report section looks like:
```
.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
filepath:line: type.value

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is `filepath:line: type.value` with `filepath` the absolute
path to the file (`.mli` if available, `.ml` otherwise) where `value` is
declared, `line` the line index in `filepath` at which `value` is declared,
`type` the path of the record or variant type within its compilation unit
(e.g. `M.t`) to which `value` belongs, and `value` the unused field or constructor.
There can be any number of such lines.

The expected resolution for an unused exported field or constructor is to remove
it from its type's definition.

The expected resolution for an unused exported value is to remove it from the
`.mli` if there is one, and the `.ml`.

> [!IMPORTANT]
> Removing unused fields or constructors may lead to compilation errors, because
> one had to write all the fields when building a record, and potentially match
> on the constructors.

# Examples

- The [code constructs](./code_constructs) directory contains a collection of
  examples dedicated to specific code constructs :
    - [Polymorphic type](./code_constructs/POLYMORPHIC_TYPE.md)
