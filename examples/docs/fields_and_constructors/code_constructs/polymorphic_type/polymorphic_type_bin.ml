(* polymorphic_type_bin.ml *)
let () =
  let open Polymorphic_type_lib in
  let left = Left 1 in
  let both = {left = 1; right = "one"} in
  match left with
  | Right x -> assert (x = both.right)
  | _ -> ()
