(* extenal uses with explicit module *)
let () =
  let _ = Values_no_intf.used_int in
  Values_no_intf.used_fun ();
  Values_no_intf.used_alias_fun ();
  let _closure = Values.partially_used_fun () in
  let ((), ()) = Values_no_intf.used_closure () () in
  Values_no_intf._used_underscore_fun ()

(* external uses using local open *)
let () =
  let open Values_no_intf in
  let _ = externally_used_int in
  externally_used_fun ();
  externally_used_alias_fun ();
  let ((), ()) = externally_used_closure () () in
  _externally_used_underscore_fun ()

(* external uses using "global" open *)
open Values_no_intf
let () =
  match externally_matched_option with
  | None -> ()
  | Some _ -> ()

let () = externally_written_bool_ref := externally_stored_in_ref_bool

let () =
  let bool_opt = Some externally_stored_in_option_bool in
  Option.iter externally_used_as_parameter_fun bool_opt

let () = ignore externally_used_as_parameter_int

let is_used = ref false
let mark_used () =
  is_used := true
