(* intext_app_bin.ml *)
let () =
  let open Intext_app_lib in
  let min = min 0 42 in
  let max = max ~min 100 200 in
  assert (min = max)
