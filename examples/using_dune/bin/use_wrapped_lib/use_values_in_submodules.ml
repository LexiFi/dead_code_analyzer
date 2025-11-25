open Wrapped_lib.Values_in_submodules

let () =
  let _ = Exported.used_int in
  ignore Exported.externally_used_int

let is_used = ref false
let mark_used () =
  is_used := true
