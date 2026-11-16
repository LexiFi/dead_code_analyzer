open Ctors_fields_in_functor
module Applied = ExternalFunctor (M)

(* external uses *)
let () =
  let _ : Applied.constructor_externally_used = Applied.C in
  let _ = fun (r : Applied.record_externally_used) -> ignore r.f in
  ()

let is_used = ref false

let mark_used () =
  is_used := true
