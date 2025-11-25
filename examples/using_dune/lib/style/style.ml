let all =
  let unit_binding = () in
  let expect_opt_arg_in_arg (f : ?opt:'a -> unit -> unit) = f () in
  let () (* sequence *) = ignore expect_opt_arg_in_arg in
  let useless_binding = 42 in
  useless_binding

