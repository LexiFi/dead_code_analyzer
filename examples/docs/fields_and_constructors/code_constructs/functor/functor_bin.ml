(* functor_bin.ml *)
open Functor_lib

module External_app = External_functor (M)

let () =
  let _ : External_app.constructor_externally_used = External_app.C in
  let _ = fun (r : External_app.record_externally_used) -> ignore r.f in
  ()
