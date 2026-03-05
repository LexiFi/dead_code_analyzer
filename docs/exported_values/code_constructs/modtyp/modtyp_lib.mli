(* modtyp_lib.mli *)
module type T = sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end

module M_reuse : T

module M_constr : T with type t = unit

module M_subst : T with type t := unit

module M_redef : sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end
