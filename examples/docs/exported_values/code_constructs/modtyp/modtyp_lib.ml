(* modtyp_lib.ml *)
module type T = sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end

module M = struct
  type t = unit
  let externally_used = ()
  let internally_used = ()
  let unused = ()
  let unused_unexported = ()
end

let () = M.internally_used

module M_reuse = M

module M_constr = M

module M_subst = M

module M_redef = M
