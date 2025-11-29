module Exported : sig
  val used_int : int
  val unused_int : int
  val internally_used_int : int
  val externally_used_int : int
end = struct
  module Private = struct
    let unused_int = 42
  end

    let used_int = 42
    let unused_int = 42
    let internally_used_int = 42
    let externally_used_int = 42

end

let () =
  let _ = Exported.used_int in
  ignore Exported.internally_used_int
