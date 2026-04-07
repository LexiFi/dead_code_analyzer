(* extensible_variant_lib.ml *)
type t = ..
type t +=
  | Int of int
  | Float of float

let to_string_opt = function
  | Int i -> Some (string_of_int i)
  | Float f -> Some (string_of_float f)
  | _ -> None
