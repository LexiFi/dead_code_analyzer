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
