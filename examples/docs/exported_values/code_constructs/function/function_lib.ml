(* function_lib.ml *)

let memoize ~f =
  let mem = Hashtbl.create 8 in
  function x ->
  match Hashtbl.find_opt mem x with
  | Some y -> y
  | None ->
    let y = f x in
    Hashtbl.add mem x y;
    y

let heavy_computation x = x

let unused x = x

let do_nothing x = ()
