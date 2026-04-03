(* relaying_bin.ml *)
let f ?x () = ignore x

let g ?x () = f ?x ()

let () =
  f ~x:42 ();
  g ()
