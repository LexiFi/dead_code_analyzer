(* module_lib.ml *)
module M = struct
  type t = unit
  let externally_used = ()
  let internally_used = ()
  let unused = ()
  let unused_unexported = ()
end

let () = M.internally_used
