(* unit.ml *)
let compute_answer input =
  let print = print_endline "Computing answer" in
  match print with
  | r when input = print -> 42
  | _ -> assert false
