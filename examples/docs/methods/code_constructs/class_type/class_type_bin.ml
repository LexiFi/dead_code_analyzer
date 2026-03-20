(* class_type_bin.ml *)
class type unused_class_type = object method unused_method : unit end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let open Class_type_lib in
  let n = 42 in
  push_n_times n int_stack;
  while int_stack#peek <> None do
    int_stack#pop;
  done
