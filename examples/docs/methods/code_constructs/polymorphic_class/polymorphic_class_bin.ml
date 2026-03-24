(* polymorphic_class_bin.ml *)
let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let int_stack = new Polymorphic_class_lib.stack in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
