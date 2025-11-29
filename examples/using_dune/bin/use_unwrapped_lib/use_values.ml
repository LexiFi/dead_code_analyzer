(* extenal uses with explicit module *)
let () =
  let _ = Values.used_int in
  Values.used_fun ();
  Values.used_alias_fun ();
  let _closure = Values.partially_used_fun () in
  let ((), ()) = Values.used_closure () () in
  Values._used_underscore_fun ()

(* external uses using local open *)
let () =
  let open Values in
  let _ = externally_used_int in
  externally_used_fun ();
  externally_used_alias_fun ();
  let ((), ()) = externally_used_closure () () in
  _externally_used_underscore_fun ()

(* external uses using "global" open *)
open Values
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
