(* modsig_lib.ml *)
module Original = struct
  let used_directly = ()
  let used_indirectly = ()
  let used_by_requirement = ()
  let unused = ()
end

module Alias_without_sig = Original

module Alias_with_sig : sig
  val used_by_requirement : unit
end = Original
