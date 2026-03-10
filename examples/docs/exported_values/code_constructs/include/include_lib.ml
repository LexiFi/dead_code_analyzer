(* include_lib.ml *)
module Original = struct
  let used_directly = ()
  let used_indirectly = ()
  let unused = ()
end

module Reexport = struct
  include Original
end

module Redefine = struct
  include Original
  let used_directly = ()
  let unused = ()
end
