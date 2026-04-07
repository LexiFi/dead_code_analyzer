(* external_app_bin.ml *)
let () =
  let open External_app_lib in
  let max = max 0 42 in
  let min = min ~max 100 200 in
  assert (min = max)
