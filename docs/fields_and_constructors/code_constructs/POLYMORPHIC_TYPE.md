The reference files for this example are in the
[polymorphic\_type](../../../examples/docs/fields_and_constructors/code_constructs/polymorphic_type) directory.

The reference takes place in `/tmp/docs/fields_and_constructors/code_constructs`, which
is a copy of the [code\_constructs](../../../examples/docs/fields_and_constructors/code_constructs)
directory. Reported locations may differ depending on the location of the source
files.

The compilation command is :
```
make -C polymorphic_type build
```

The analysis command is :
```
make -C polymorphic_type analyze
```

The compile + analyze command is :
```
make -C polymorphic_type
```

## First run

Code:
```OCaml
(* polymorphic_type_lib.mli *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) both = {left : 'a; right : 'b}
```
```OCaml
(* polymorphic_type_lib.ml *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) both = {left : 'a; right : 'b}
```
```OCaml
(* polymorphic_type_bin.ml *)
let () =
  let open Polymorphic_type_lib in
  let left = Left 1 in
  let both = {left = 1; right = "one"} in
  match left with
  | Right x -> assert (x = both.right)
  | _ -> ()
```

Before looking at the analysis results, let's look at the code.

The `Polymorphic_type_lib` exports 1 variant type `either` with 2 constructors
`Left` and `Right, and 1 record `both` with 2 fields `left` and `right.

The constructor `Left` is used to build the value stored in `left`.
The constructor `Right` is never built but matched upon. Thus, `Left` is used
while `Right` is not.
Both the fields `both.left` and `both.right` are written bu only the field
`both.right` is read. This leaves `both.left` as unused.

Compiler and analyze :
```
$ make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
ocamlopt -w +37+69 -bin-annot polymorphic_type_lib.mli polymorphic_type_lib.ml polymorphic_type_bin.ml
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type/polymorphic_type_lib.mli:2: either.Right
/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type/polymorphic_type_lib.mli:3: both.left

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
```

As expected, constructor `either.Right` and field `both.left` are reported by
the analyzer.

## Removing the unused constructor and field

> [!TIP]
> Do not forget to remove `either.Right` from both the `.mli` **and** the `.ml`.
> Otherwise, the compiler will reject the code with a message like :
> ```
> File "polymorphic_type_lib.ml", line 1:
> Error: The implementation polymorphic_type_lib.ml
>        does not match the interface polymorphic_type_lib.mli:
>        Type declarations do not match:
>          type ('a, 'b) either = Left of 'a | Right of 'b
>        is not included in
>          type ('a, 'b) either = Left of 'a
>        An extra constructor, Right, is provided in the first declaration.
>        File "polymorphic_type_lib.mli", line 2, characters 0-33:
>          Expected declaration
>        File "polymorphic_type_lib.ml", line 2, characters 0-47:
>          Actual declaration
> ```

> [!TIP]
> Do not forget to remove `both.left` from both the `.mli` **and** the `.ml`.
> Otherwise, the compiler will reject the code with a message like :
> ```
> Error: The implementation polymorphic_type_lib.ml
>        does not match the interface polymorphic_type_lib.mli:
>        Type declarations do not match:
>          type ('a, 'b) both = { left : 'a; right : 'b; }
>        is not included in
>          type ('a, 'b) both = { right : 'b; }
>        An extra field, left, is provided in the first declaration.
>        File "polymorphic_type_lib.mli", line 3, characters 0-33:
>          Expected declaration
>        File "polymorphic_type_lib.ml", line 3, characters 0-44:
>          Actual declaration
> ```

After removing the unused constructor and field, the compiler will report errors
at the locations were they are de-structured and written respectively.
Fixing those errors is simply done by removing the invalid pattern branches and
writes.

Compile :
```
$ make -C polymorphic_type build
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
ocamlopt -w +37+69 -bin-annot polymorphic_type_lib.mli polymorphic_type_lib.ml polymorphic_type_bin.ml
File "polymorphic_type_bin.ml", line 5, characters 14-18:
5 |   let both = {left = 1; right = "one"} in
                  ^^^^
Error: Unbound record field left
make: *** [Makefile:6: build] Error 2
make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
```
Fix and compile :
```
$ make -C polymorphic_type build
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
ocamlopt -w +37+69 -bin-annot polymorphic_type_lib.mli polymorphic_type_lib.ml polymorphic_type_bin.ml
File "polymorphic_type_bin.ml", line 7, characters 4-9:
7 |   | Right x -> assert (x = both.right)
        ^^^^^
Error: This variant pattern is expected to have type
         (int, 'a) Polymorphic_type_lib.either
       There is no constructor Right within type Polymorphic_type_lib.either
make: *** [Makefile:6: build] Error 2
make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
```
Fix and compile :
```
$ make -C polymorphic_type build
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
ocamlopt -w +37+69 -bin-annot polymorphic_type_lib.mli polymorphic_type_lib.ml polymorphic_type_bin.ml
File "polymorphic_type_bin.ml", line 5, characters 6-10:
5 |   let both = {right = "one"} in
          ^^^^
Warning 26 [unused-var]: unused variable both.
make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
```

Compilation succeeds without error. There is a warning on an unused variable.
This topic is discussed in the
[Exported values](../exported_values/EXPORTED_VALUES.md) section.

Now we have removed the unused constructor and field, and fixed all the
compilation errors. Let's look at the code and analyze it.

Code :
```OCaml
(* polymorphic_type_lib.mli *)
type ('a, 'b) either = Left of 'a
type ('a, 'b) both = {right : 'b}
```
```OCaml
(* polymorphic_type_lib.ml *)
type ('a, 'b) either = Left of 'a
type ('a, 'b) both = {right : 'b}
```
```OCaml
(* polymorphic_type_bin.ml *)
let () =
  let open Polymorphic_type_lib in
  let left = Left 1 in
  let both = {right = "one"} in
  match left with
  | _ -> ()
```

Analyze :
```
$ make -C polymorphic_type analyze
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================
/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type/polymorphic_type_lib.mli:3: both.right

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
```

The analyzer reports `both.right` as unused. This is coherent with the compiler
warning above saying that variable `both` is unused  because it is the only
value of type `both`.

Let's fix both the compiler warning and the analyzer's report.

## Removing the unused value and field

Because `right` is the only field in `both`, removing the field necessitates
either removing the type or making the type abstract. We will remove the type
from the `.mli`

> [!NOTE]
> An empty record type is a syntax error :
> ```
> File "polymorphic_type_lib.mli", line 3, characters 23-24:
> 3 | type ('a, 'b) both = { }
>                            ^
> Error: Syntax error
> ```

Code :
```OCaml
(* polymorphic_type_lib.mli *)
type ('a, 'b) either = Left of 'a
```
```OCaml
(* polymorphic_type_lib.ml *)
type ('a, 'b) either = Left of 'a
type ('a, 'b) both = {right : 'b}
```
```OCaml
(* polymorphic_type_bin.ml *)
let () =
  let open Polymorphic_type_lib in
  let left = Left 1 in
  match left with
  | _ -> ()
```

Compile and analyze :
```
$ make -C polymorphic_type
make: Entering directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
ocamlopt -w +37+69 -bin-annot polymorphic_type_lib.mli polymorphic_type_lib.ml polymorphic_type_bin.ml
File "polymorphic_type_lib.ml", line 3, characters 22-32:
3 | type ('a, 'b) both = {right : 'b}
                          ^^^^^^^^^^
Warning 69 [unused-field]: unused record field right.
dead_code_analyzer --nothing -T all .
Scanning files...
 [DONE]

.> UNUSED CONSTRUCTORS/RECORD FIELDS:
====================================

Nothing else to report in this section
--------------------------------------------------------------------------------


make: Leaving directory '/tmp/docs/fields_and_constructors/code_constructs/polymorphic_type'
```

The compiler warns us that the unuexported field `right` is unused.
The analyzer does not report anything.

Once again, the fix is to either make the type abstract or removing it from
the `.ml`. We can do the latter and neither the compiler nor the analyzer will
report any unused constructor or field. Our work here is done.

> [!TIP]
> If we activated the compiler warning 34 `unused-type-declaration`
> (by passing the argument `-w +34`), and made `both` abstract in the `.ml`,
> then the compiler would have reported.
> ```
> File "polymorphic_type_lib.ml", line 3, characters 0-18:
> 3 | type ('a, 'b) both
>     ^^^^^^^^^^^^^^^^^^
> Warning 34 [unused-type-declaration]: unused type both.
> ```
> The analyzer does not have an unused exported type feature yet so making the
> type abstract in the `.mli` would not have lead to any unused exported type
> report.
