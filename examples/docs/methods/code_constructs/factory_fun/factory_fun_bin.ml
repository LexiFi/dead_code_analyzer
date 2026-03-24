(* factoy_fun_bin.ml *)
let unused_factory () = object method unused_method = () end

let push_n_times n stack =
  for i = 1 to n do
    stack#push i;
  done

let () =
  let open Factory_fun_lib in
  let n = 42 in
  push_n_times n (get_stack ());
  while (get_stack ())#peek <> None do
    (get_stack ())#pop;
  done
