open Wrapped_lib

let () =
  let open Values_in_first_class_module in
  let _ = (module M : VAL_CSTR) in
  let _ : (module VAL_REQ) = (module M) in
  expect_param (module M)

let () =
  let open Values_in_first_class_module_no_intf in
  let _ = (module M : VAL_CSTR) in
  let _ : (module VAL_REQ) = (module M) in
  expect_param (module M)

let is_used = ref false
let mark_used () =
  is_used := true
