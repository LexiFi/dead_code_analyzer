let () = ignore Wrapped_lib.Style.all

let is_used = ref false
let mark_used () =
  is_used := true
