(* extenal uses with explicit module *)
let () =
  let _ = Wrapped_lib.Values_no_intf.used_int in
  Wrapped_lib.Values_no_intf.used_fun ();
  Wrapped_lib.Values_no_intf.used_alias_fun ();
  let _closure = Wrapped_lib.Values_no_intf.partially_used_fun () in
  let ((), ()) = Wrapped_lib.Values_no_intf.used_closure () () in
  Wrapped_lib.Values_no_intf._used_underscore_fun ()

(* external uses using local open *)
let () =
  let open Wrapped_lib in
  let _ = Values_no_intf.externally_used_int in
  Values_no_intf.externally_used_fun ();
  let open Values_no_intf in
  externally_used_alias_fun ();
  let ((), ()) = externally_used_closure () () in
  _externally_used_underscore_fun ()

(* external uses using "global" open *)
open Wrapped_lib
let () =
  match Values_no_intf.externally_matched_option with
  | None -> ()
  | Some _ -> ()

let () =
  Values_no_intf.externally_written_bool_ref := Values_no_intf.externally_stored_in_ref_bool

open Values_no_intf
let () =
  let bool_opt = Some externally_stored_in_option_bool in
  Option.iter externally_used_as_parameter_fun bool_opt

let () = ignore externally_used_as_parameter_int

let is_used = ref false
let mark_used () =
  is_used := true
