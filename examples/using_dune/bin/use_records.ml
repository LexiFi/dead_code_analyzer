(* extenal uses with explicit module *)
let () =
  let _ = fun r -> r.Records.used_single_unit in
  let _ = fun r -> ignore r.Records.used_single_param in
  let _ = fun r ->
    ignore r.Records.used_param;
    r.Records.used_unit
  in
  ()

(* external uses using local open *)
let () =
  let open Records in
  let _ = fun r -> r.externally_used_single_unit in
  let _ = fun r -> ignore r.externally_used_single_param in
  let _ = fun r ->
    ignore r.externally_used_param;
    r.externally_used_unit
  in
  ()

(* external uses using "global" open *)
open Records
let () =
  let _ = fun (r : 'a partially_used_mix_1) -> ignore r.partially_used_param in
  let _ = fun (r : 'a partially_used_mix_2) -> r.partially_used_unit in
  let _ = function
    | ({partially_used_in_match_param; _} : 'a partially_used_in_match) ->
      ignore partially_used_in_match_param
  in
  ()

let is_used = ref false
let mark_used () =
  is_used := true
