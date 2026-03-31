(* inline_record_lib.ml *)
type ('a, 'b) t =
  | Both of {left : 'a; right : 'b}
  | Left of 'a
  | Right of 'b

let get_left_opt = function
  | Both {left; _}
  | Left left -> Some left
  | _ -> None
