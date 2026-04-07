(* external_app_lib.ml *)
let max ?min x y =
  let max x y = if x > y then x else y in
  let res = max x y in
  match min with
  | None -> res
  | Some m -> max m res

let min ?max x y =
  let min x y = if x < y then x else y in
  let res = min x y in
  match max with
  | None -> res
  | Some m -> min m res
