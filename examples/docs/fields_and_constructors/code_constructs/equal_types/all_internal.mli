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
