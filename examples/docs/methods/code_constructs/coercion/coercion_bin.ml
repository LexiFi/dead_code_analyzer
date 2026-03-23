(* coercion_bin.ml *)
open Coercion_lib

let () =
  let _coerce = (obj :> < used_by_requirement : unit >) in
  ()
