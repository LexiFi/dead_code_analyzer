(* total_app_bin.ml *)
let sum ?x ?y () =
  match x, y with
  | Some x, Some y -> x + y
  | Some x, None -> x
  | None, Some y -> y
  | None, None -> 0

let () =
  let x = 42 in
  let x' = sum ~x () in
  assert (x' = x)
