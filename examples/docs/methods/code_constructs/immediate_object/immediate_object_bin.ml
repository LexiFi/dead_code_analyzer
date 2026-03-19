(* immediate_object_bin.ml *)
let unused_obj = object method unused_method = () end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let open Immediate_object_lib in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
