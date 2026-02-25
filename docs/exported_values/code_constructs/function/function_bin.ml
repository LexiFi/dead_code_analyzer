(* function_bin.ml *)

let () =
  let my_memoized = Function_lib.(memoize ~f:heavy_computation) in
  Function_lib.do_nothing ();
  assert (my_memoized 42 = my_memoized 42)
