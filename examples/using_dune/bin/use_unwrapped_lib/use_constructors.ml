(* extenal uses with explicit module *)
let () =
  let _ = Constructors.Used_single_no_param in
  let _ = Constructors.Used_single_unit () in
  let _ = Constructors.Used_single_param 42 in
  let _ = Constructors.Used_no_param in
  let _ = Constructors.Used_unit () in
  let _ = Constructors.Used_param 42 in
  ()

(* external uses using local open *)
let () =
  let open Constructors in
  let _ = Externally_used_single_no_param in
  let _ = Externally_used_single_unit () in
  let _ = Externally_used_single_param 42 in
  let _ = Externally_used_no_param in
  let _ = Externally_used_unit () in
  let _ = Externally_used_param 42 in
  ()

(* external uses using "global" open *)
open Constructors
let () =
  let _ : _ partially_used_mix_1 = Partially_used_param 42 in
  let _ : _ partially_used_mix_2 = Partially_used_no_param in
  let _ : _ partially_used_mix_3 = Partially_used_unit () in
  ()

let is_used = ref false
let mark_used () =
  is_used := true
