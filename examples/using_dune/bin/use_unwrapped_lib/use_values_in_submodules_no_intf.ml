open Values_in_submodules_no_intf

let () =
  let _ = Exported.used_int in
  ignore Exported.externally_used_int

let is_used = ref false
let mark_used () =
  is_used := true
