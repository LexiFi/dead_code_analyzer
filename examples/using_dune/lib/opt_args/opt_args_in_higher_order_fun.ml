let partially_applied_with_never_used_opt_arg ?never () () =
  ignore never

let partially_applied_with_always_used_opt_arg ?always () () =
  ignore always

let partially_applied_with_sometimes_used_opt_arg ?sometimes () () =
  ignore sometimes

let shadowed_never_used_opt_arg ?duplicate () =
  ignore duplicate
let shadowing_never_used_arg ?duplicate =
  shadowed_never_used_opt_arg

let shadowed_always_used_opt_arg ?duplicate () =
  ignore duplicate
let shadowing_always_used_arg ?duplicate =
  shadowed_always_used_opt_arg

let expect_fun_with_never_used_opt_arg f =
  f ?type_used:None ()
let expect_fun_with_always_used_opt_arg f =
  f ?type_used:(Some ()) ()

let expect_fun_with_never_used_opt_arg_with_sig (f : ?type_used:'a -> unit -> unit) =
  f ?type_used:None ()
let expect_fun_with_always_used_opt_arg_with_sig (f : ?type_used:'a -> unit -> unit) =
  f ?type_used:(Some ()) ()

let fun_with_always_type_used_opt_arg ?type_used () =
  ignore type_used

let () =
  let _ = partially_applied_with_never_used_opt_arg () in
  let _ = partially_applied_with_always_used_opt_arg ~always:() () in
  let _ = partially_applied_with_sometimes_used_opt_arg () in
  let _ = partially_applied_with_sometimes_used_opt_arg ~sometimes:() () in
  shadowing_never_used_arg ~duplicate:() ();
  shadowing_always_used_arg ~duplicate:() ~duplicate:() ();
  expect_fun_with_never_used_opt_arg fun_with_always_type_used_opt_arg;
  expect_fun_with_always_used_opt_arg fun_with_always_type_used_opt_arg;
  expect_fun_with_never_used_opt_arg_with_sig fun_with_always_type_used_opt_arg;
  expect_fun_with_always_used_opt_arg_with_sig fun_with_always_type_used_opt_arg;
  ()
