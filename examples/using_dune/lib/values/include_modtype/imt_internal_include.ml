module type S = sig
  val unused : int
  val used : int
  val internally_used :int
  val externally_used :int
end

let unused = 42
let used = 42
let internally_used = 42
let externally_used = 42
let sometimes_used = 42

let () =
  ignore used;
  ignore internally_used;
  ignore sometimes_used
