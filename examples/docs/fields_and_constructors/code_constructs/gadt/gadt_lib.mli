(* gadt_lib.mli *)
type _ gadt =
  | Int : int -> int gadt
  | Float : float -> float gadt

val gadt_of_int : int -> int gadt

val float_opt_of_gadt : 'a gadt -> float option
