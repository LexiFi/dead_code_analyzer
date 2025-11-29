module Unexported = struct
  let used_int = 42
  let unused_int = 42
end

module Exported = struct
  module Private = struct
    let used_int = 42
    let unused_int = 42
  end

    let used_int = 42
    let unused_int = 42
    let internally_used_int = 42
    let externally_used_int = 42

end

let () = ignore Unexported.used_int

let () = ignore Exported.Private.used_int

let () =
  let _ = Exported.used_int in
  ignore Exported.internally_used_int
