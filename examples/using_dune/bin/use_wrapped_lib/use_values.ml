(* extenal uses with explicit module *)
let () =
  let _ = Wrapped_lib.Values.used_int in
  Wrapped_lib.Values.used_fun ();
  Wrapped_lib.Values.used_alias_fun ();
  let _closure = Wrapped_lib.Values.partially_used_fun () in
  let ((), ()) = Wrapped_lib.Values.used_closure () () in
  Wrapped_lib.Values._used_underscore_fun ()

(* external uses using local open *)
let () =
  let open Wrapped_lib in
  let _ = Values.externally_used_int in
  Values.externally_used_fun ();
  let open Values in
  externally_used_alias_fun ();
  let ((), ()) = externally_used_closure () () in
  _externally_used_underscore_fun ()

(* external uses using "global" open *)
open Wrapped_lib
let () =
  match Values.externally_matched_option with
  | None -> ()
  | Some _ -> ()

let () =
  Values.externally_written_bool_ref := Values.externally_stored_in_ref_bool

open Values
let () =
  let bool_opt = Some externally_stored_in_option_bool in
  Option.iter externally_used_as_parameter_fun bool_opt

let () = ignore externally_used_as_parameter_int

let is_used = ref false
let mark_used () =
  is_used := true
