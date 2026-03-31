(* polymorphic_variant_lib.ml *)
type poly = [`Int of int | `Float of float]

let poly_of_int x = `Int x

let float_opt_of_poly = function
  | `Float f -> Some f
  | _ -> None
