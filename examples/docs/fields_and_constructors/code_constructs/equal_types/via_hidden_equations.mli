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

