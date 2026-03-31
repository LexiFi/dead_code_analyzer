(* gadt_bin.ml *)
let () =
  let open Gadt_lib in
  let x = 0 in
  let gadt = gadt_of_int x in
  let f = float_opt_of_gadt gadt in
  assert (f = None)
