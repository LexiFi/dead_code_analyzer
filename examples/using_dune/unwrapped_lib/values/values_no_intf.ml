let used_int = 42
let unused_int = 42
let internally_used_int = 42
let externally_used_int = 42
let externally_used_as_parameter_int = 42

let externally_written_bool_ref = ref false
let externally_matched_option = Some false

let externally_stored_in_ref_bool = false
let externally_stored_in_option_bool = false

let unused_fun x = x
let used_fun x = x
let internally_used_fun x = x
let externally_used_fun x = x
let externally_used_as_parameter_fun _ = ()

let aliased_fun x = x
let unused_alias_fun = aliased_fun
let used_alias_fun = aliased_fun
let externally_used_alias_fun = aliased_fun

let partially_used_fun x y = (x, y)
let unused_closure x = partially_used_fun x
let used_closure x = partially_used_fun x
let internally_used_closure x = partially_used_fun x
let externally_used_closure x = partially_used_fun x

let _used_underscore_fun x = x
let _unused_underscore_fun x = x
let _externally_used_underscore_fun x = x

let () =
  let _ = used_int in
  let _ = internally_used_int in
  used_fun ();
  internally_used_fun ();
  used_alias_fun ();
  let _closure = partially_used_fun () in
  let closure = used_closure () in
  let ((), ()) = closure () in
  let ((), ()) = internally_used_closure () () in
  _used_underscore_fun ()
