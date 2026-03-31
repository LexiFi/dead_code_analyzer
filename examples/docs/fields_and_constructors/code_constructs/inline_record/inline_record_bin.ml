(* inline_record_bin.ml *)
let () =
  let open Inline_record_lib in
  let both = Both {left = 1; right = "one"} in
  match get_left_opt both with
  | Some left -> assert (left = 1)
  | _ -> assert false
