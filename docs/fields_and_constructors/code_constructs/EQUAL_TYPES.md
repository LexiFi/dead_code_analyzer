The reference files for this example are in the
[equal\_types](../../../examples/docs/fields_and_constructors/code_constructs/equal_types)
directory.

The reference takes place in `/tmp/docs/fields_and_constructors/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/fields_and_constructors/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C equal_types build
```

The analysis command is :
```
make -C equal_types analyze
```

The compile + analyze command is :
```
make -C equal_types
```

## First run

There is a lot of code involved. Feel free to skip it and jump to its
[description](#code_description).

Code:
```OCaml
(* definitions.ml *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Unused

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  unused : int;
}
```
```OCaml
(* via_explicit_equations.ml *)
type sum = Definitions.sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Unused

type product = Definitions.product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  unused : int;
}
```
```OCaml
(* via_hidden_equations.mli *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Unused

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  unused : int;
}
```
```OCaml
(* via_hidden_equations.ml *)
type sum = Definitions.sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Unused

type product = Definitions.product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  unused : int;
}
```
```OCaml
(* via_include.mli *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Unused

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  unused : int;
}
```
```OCaml
(* via_include.ml *)
include Definitions
```
```OCaml
(* via_module_alias.mli *)
module Alias : sig
  type sum =
    | Used_by_explicit_equation
    | Used_by_hidden_equation
    | Used_by_include
    | Used_by_module_alias
    | Used_directly
    | Unused

  type product = {
    used_by_explicit_equation : int;
    used_by_hidden_equation : int;
    used_by_include : int;
    used_by_module_alias : int;
    used_directly : int;
    unused : int;
  }
end
```
```OCaml
(* via_module_alias.ml *)
module Alias = Definitions
```
```OCaml
(* all_internal.mli *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Used_externally
  | Unused

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  used_externally : int;
  unused : int;
}
```
```OCaml
(* all_internal.ml *)
type original_sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Used_externally
  | Unused

type original_product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  used_externally : int;
  unused : int;
}

let _ : original_product -> original_sum =
  fun {used_directly; _} -> Used_directly

type explicit_eq_sum = original_sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Used_externally
  | Unused

type explicit_eq_product = original_product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  used_externally : int;
  unused : int;
}

let _ : explicit_eq_product -> explicit_eq_sum =
  fun {used_by_explicit_equation; _} -> Used_by_explicit_equation

module M : sig
  type sum =
    | Used_by_explicit_equation
    | Used_by_hidden_equation
    | Used_by_include
    | Used_by_module_alias
    | Used_directly
    | Used_externally
    | Unused

  type product = {
    used_by_explicit_equation : int;
    used_by_hidden_equation : int;
    used_by_include : int;
    used_by_module_alias : int;
    used_directly : int;
    used_externally : int;
    unused : int;
  }
end = struct
  type sum = explicit_eq_sum =
    | Used_by_explicit_equation
    | Used_by_hidden_equation
    | Used_by_include
    | Used_by_module_alias
    | Used_directly
    | Used_externally
    | Unused

  type product = explicit_eq_product = {
    used_by_explicit_equation : int;
    used_by_hidden_equation : int;
    used_by_include : int;
    used_by_module_alias : int;
    used_directly : int;
    used_externally : int;
    unused : int;
  }
end

let _ : M.product -> M.sum =
  fun {used_by_hidden_equation; _} -> Used_by_hidden_equation

module Alias = M

let _ : Alias.product -> Alias.sum =
  fun {used_by_module_alias; _} -> Used_by_module_alias

include Alias

let _ : product -> sum =
  fun {used_by_include; _} -> Used_by_include
```
```OCaml
(* equal_types.ml *)
let _ =
  let open Definitions in
  fun {used_directly; _} -> Used_directly

let _ =
  let open Via_explicit_equations in
  fun {used_by_explicit_equation; _} -> Used_by_explicit_equation

let _ =
  let open Via_hidden_equations in
  fun {used_by_hidden_equation; _} -> Used_by_hidden_equation

let _ =
  let open Via_include in
  fun {used_by_include; _} -> Used_by_include

let _ =
  let open Via_module_alias.Alias in
  fun {used_by_module_alias; _} -> Used_by_module_alias

let _ =
  let open All_internal in
  fun {used_externally; _} -> Used_externally
```

<a id="code_description"></a>
Before looking at the analysis results, let's look at the code.

`Definitions` exports 2 types: `sum`, a regular variant type, and
`product`, a regular record type.
These 2 types exported by `Definitions` are re-exposed by 4 modules :
`Via_explicit_equations`, `Via_hidden_equations`, `Via_include`, and
`Via_module_alias`. The types exported by each of these modules are equal
to the ones in `Definitions` (thus, they are also all equal):
-   `Via_explicit_equations` is defined within a single .ml and it uses
    type equations:
    ```OCaml
    type sum = Definitions.sum = (* constructors *)
    type product = Definitions.product = (* fields *)
    ```
-   `Via_hidden_equations` is implemented the same as `Via_explicit_equations`
    in its .ml, and its .mli is the same as `Definitions`. The explicit
    equality is hidden by the interface.
-   `Via_include` is implemented as a simple inclusion of `Definitions`
    and its interface is the same as `Definitions`. The equality is implied
    but hidden by the interface.
-   `Via_module_alias` contains a single module `Alias` implemented as an
    alias of `Definitions` but, in the interface, its signature is explicit
    and the same as `Definitions`. The explicit equality is once again
    hidden by the interface.

The constructors and fields of the exported types are used in `Equal_types`.
Each one states through which module they are used.
E.g. `sum.Used_by_include` is used through `Via_include`, and
`product.used_directly` is used through `Definitions`.

`All_internal` implements all the above mentioned equalities and uses within
a single compilation unit. Its `sum` has one additional constructor
`Used_externally`, and `product` one additional field `used_externally`.
They are used in `Equal_types` instead of `sum.Used_directly` and
`product.used_directly`, which are used internally through the original
types definitions.

Overall, all the fields and constructors are used (through the different
equivalent types) except for `sum.Unused` and `product.unused`.


Compile and analyze :
```
$ make -C equal_types
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/equal_types'
ocamlopt -bin-annot definitions.ml via_explicit_equations.ml via_hidden_equations.mli via_hidden_equations.ml via_include.mli via_include.ml via_module_alias.mli via_module_alias.ml all_internal.mli all_internal.ml equal_types.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
/tmp/docs/fields_and_constructors/code_constructs/equal_types/all_internal.mli:9: sum.Unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/all_internal.mli:18: product.unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/definitions.ml:8: sum.Unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/definitions.ml:16: product.unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/via_hidden_equations.mli:8: sum.Unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/via_hidden_equations.mli:16: product.unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/via_include.mli:8: sum.Unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/via_include.mli:16: product.unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/via_module_alias.mli:9: Alias.sum.Unused
/tmp/docs/fields_and_constructors/code_constructs/equal_types/via_module_alias.mli:17: Alias.product.unused

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/equal_types'
```

As expected, only the constructor `sum.Unused` and field `product.unused`
are reported. However, they are reported for each module that exports them
and hides the equality.
According to OCaml's [documentation](https://ocaml.org/manual/5.3/typedecl.html):
> The optional type equation `= typexpr` makes the defined type equivalent
> to the type expression `typexpr`: one can be substituted for the other
> during typing. If no type equation is given, a new type is generated:
> the defined type is incompatible with any other type.

Because all the exported types are equivalent, reporting the unused fields
and constructors for one definition would suffice. In addition, reporting
equivalent components breaks the "independence" of reports: one report cannot
be addressed without also addressing the others.\
But because the equality is hidden in the signature, they must still be
considered as different definitions.\
In order to balance these two semantics and provide actionable reports,
the analyzer always reports a type's component, unless it explcitly is
exported as equivalent to another, and unifies all the equivalent
components uses to avoid reporting one that would be unused in a
specific type but used in another (e.g. `Definitions.sum.Used_by_include`
vs `Via_include.sum.Used_by_include`).

> [!NOTE]
> Unifying uses avoids reporting a component for one type when it is used
> in another equivalent type. Such reports are considered false positives
> because one cannot remove the component from one type without breaking the
> equality (which would lead to a more complex update than a dead code
> clean up)
>
> If you have a strong need/desire for separate tracking of uses and
> having such reports, please feel free to
> [open an issue](https://github.com/LexiFi/dead_code_analyzer/issues/new)

## Removing the unused constructors and fields

> [!TIP]
> When removing one component, we must remove it from all the equivalent
> types. Otherwise, the compiler will complain with an error.
> E.g. if we only remove `Via_include.sum.Unused`, the compiler tells us
> the type equation `type sum = Definitions.sum` (implied by the include)
> is invalid:
> ```
> File "via_include.ml", line 1:
> Error: The implementation via_include.ml
>       does not match the interface via_include.mli:
>       Type declarations do not match:
>         type sum =
>           Definitions.sum =
>             Used_by_explicit_equation
>           | Used_by_hidden_equation
>           | Used_by_include
>           | Used_by_module_alias
>           | Used_directly
>           | Unused
>       is not included in
>         type sum =
>             Used_by_explicit_equation
>           | Used_by_hidden_equation
>           | Used_by_include
>           | Used_by_module_alias
>           | Used_directly
>       An extra constructor, Unused, is provided in the first declaration.
>       File "via_include.mli", lines 2-7, characters 0-17:
>         Expected declaration
>       File "definitions.ml", lines 2-8, characters 0-10: Actual declaration
> ```

After removing the unused constructors and fields from the .mli and .ml files,
the compiler reports the following error:
```
File "via_explicit_equations.ml", lines 2-8, characters 0-10:
2 | type sum = Definitions.sum =
3 |   | Used_by_explicit_equation
4 |   | Used_by_hidden_equation
5 |   | Used_by_include
6 |   | Used_by_module_alias
7 |   | Used_directly
8 |   | Unused
Error: This variant or record definition does not match that of type
         Definitions.sum
       A constructor, Unused, is missing in the original definition.
```

As explained, the components in `Via_explicit_equations` were not reported
because they are explicitly exported as equal to `Definitions`'s.
Although they were not reported, the typing semantics tell us that they can
be substituted by their equivalents, so if an equivalent component is
unused, `Via_explicit_equations`'s component is.
We can simply remove the missing constructor `Unused` (and field `unused`)
from `Via_explicit_equations.sum` (and `product`)

Similarly for `All_internal`, we can follow the compiler errors to fix each
equivalent type definition.

Now we have removed the unused constructors and fields, and fixed all the
compilation errors. Let's look at the code and analyze it.

Again, there is a lot of code involved. Feel free to skip it and jump the
[analysis results](#clean_analysis).

Code:
```OCaml
(* definitions.ml *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
}
```
```OCaml
(* via_explicit_equations.ml *)
type sum = Definitions.sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly

type product = Definitions.product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
}
```
```OCaml
(* via_hidden_equations.mli *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
}
```
```OCaml
(* via_hidden_equations.ml *)
type sum = Definitions.sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly

type product = Definitions.product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
}
```
```OCaml
(* via_include.mli *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
}
```
```OCaml
(* via_include.ml *)
include Definitions
```
```OCaml
(* via_module_alias.mli *)
module Alias : sig
  type sum =
    | Used_by_explicit_equation
    | Used_by_hidden_equation
    | Used_by_include
    | Used_by_module_alias
    | Used_directly

  type product = {
    used_by_explicit_equation : int;
    used_by_hidden_equation : int;
    used_by_include : int;
    used_by_module_alias : int;
    used_directly : int;
  }
end
```
```OCaml
(* via_module_alias.ml *)
module Alias = Definitions
```
```OCaml
(* all_internal.mli *)
type sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Used_externally

type product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  used_externally : int;
}
```
```OCaml
(* all_internal.ml *)
type original_sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Used_externally

type original_product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  used_externally : int;
}

let _ : original_product -> original_sum =
  fun {used_directly; _} -> Used_directly

type explicit_eq_sum = original_sum =
  | Used_by_explicit_equation
  | Used_by_hidden_equation
  | Used_by_include
  | Used_by_module_alias
  | Used_directly
  | Used_externally

type explicit_eq_product = original_product = {
  used_by_explicit_equation : int;
  used_by_hidden_equation : int;
  used_by_include : int;
  used_by_module_alias : int;
  used_directly : int;
  used_externally : int;
}

let _ : explicit_eq_product -> explicit_eq_sum =
  fun {used_by_explicit_equation; _} -> Used_by_explicit_equation

module M : sig
  type sum =
    | Used_by_explicit_equation
    | Used_by_hidden_equation
    | Used_by_include
    | Used_by_module_alias
    | Used_directly
    | Used_externally

  type product = {
    used_by_explicit_equation : int;
    used_by_hidden_equation : int;
    used_by_include : int;
    used_by_module_alias : int;
    used_directly : int;
    used_externally : int;
  }
end = struct
  type sum = explicit_eq_sum =
    | Used_by_explicit_equation
    | Used_by_hidden_equation
    | Used_by_include
    | Used_by_module_alias
    | Used_directly
    | Used_externally

  type product = explicit_eq_product = {
    used_by_explicit_equation : int;
    used_by_hidden_equation : int;
    used_by_include : int;
    used_by_module_alias : int;
    used_directly : int;
    used_externally : int;
  }
end

let _ : M.product -> M.sum =
  fun {used_by_hidden_equation; _} -> Used_by_hidden_equation

module Alias = M

let _ : Alias.product -> Alias.sum =
  fun {used_by_module_alias; _} -> Used_by_module_alias

include Alias

let _ : product -> sum =
  fun {used_by_include; _} -> Used_by_include
```
```OCaml
(* equal_types.ml *)
let _ =
  let open Definitions in
  fun {used_directly; _} -> Used_directly

let _ =
  let open Via_explicit_equations in
  fun {used_by_explicit_equation; _} -> Used_by_explicit_equation

let _ =
  let open Via_hidden_equations in
  fun {used_by_hidden_equation; _} -> Used_by_hidden_equation

let _ =
  let open Via_include in
  fun {used_by_include; _} -> Used_by_include

let _ =
  let open Via_module_alias.Alias in
  fun {used_by_module_alias; _} -> Used_by_module_alias

let _ =
  let open All_internal in
  fun {used_externally; _} -> Used_externally
```

<a id="clean_analysis"></a>

Compile and analyze :
```
$ make -C equal_types
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/equal_types'
ocamlopt -bin-annot definitions.ml via_explicit_equations.ml via_hidden_equations.mli via_hidden_equations.ml via_include.mli via_include.ml via_module_alias.mli via_module_alias.ml all_internal.mli all_internal.ml equal_types.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/equal_types'
```

The analyzer does not report any unused constructor or field.
Our work here is done.
