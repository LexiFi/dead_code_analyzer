(* polymorphic_variant_lib.mli *)
type poly = [`Int of int | `Float of float]

val poly_of_int : int -> [> `Int of int]

val float_opt_of_poly : poly -> float option
