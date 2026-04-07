(* extensible_variant_lib.mli *)
type t = ..
type t +=
  | Int of int
  | Float of float

val to_string_opt : t -> string option
