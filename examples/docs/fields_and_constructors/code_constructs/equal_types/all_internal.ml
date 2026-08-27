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
