(* gadt_lib.ml *)
type _ gadt =
  | Int : int -> int gadt
  | Float : float -> float gadt

let gadt_of_int x = Int x

let float_opt_of_gadt : type a . a gadt -> float option = function
  | Float f -> Some f
  | _ -> None
