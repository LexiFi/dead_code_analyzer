let () =
  let open Opt_args_in_higher_order_fun in
  let pa = partially_applied_with_never_used_opt_arg () in
  ignore pa;
  let pa = partially_applied_with_always_used_opt_arg ~always:() () in
  ignore pa;
  let pa = partially_applied_with_sometimes_used_opt_arg () in
  ignore pa;
  let pa = partially_applied_with_sometimes_used_opt_arg ~sometimes:() () in
  ignore pa;
  shadowing_never_used_arg ~duplicate:() ();
  shadowing_always_used_arg ~duplicate:() ~duplicate:() ();
  expect_fun_with_never_used_opt_arg fun_with_always_type_used_opt_arg;
  expect_fun_with_always_used_opt_arg fun_with_always_type_used_opt_arg;
  expect_fun_with_never_used_opt_arg_with_sig fun_with_always_type_used_opt_arg;
  expect_fun_with_always_used_opt_arg_with_sig fun_with_always_type_used_opt_arg;
  ()

let is_used = ref false
let mark_used () =
  is_used := true
