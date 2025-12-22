let () =
  Use_preprocessed.mark_used ();
  Use_preprocessed_no_intf.mark_used ()

let is_used = ref false
let mark_used () =
  is_used := true
