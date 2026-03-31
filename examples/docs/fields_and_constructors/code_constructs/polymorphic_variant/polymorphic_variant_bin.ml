(* polymorphic_variant_bin.ml *)
let () =
  let open Polymorphic_variant_lib in
  let x = 0 in
  let poly = poly_of_int x in
  let f =  float_opt_of_poly poly in
  assert (f = None)
