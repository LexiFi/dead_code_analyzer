(* functor_lib.mli *)
type t

module F (P : sig
  val used_required : t
  val unused_required : t
end) : sig
  val externally_used : t
  val internally_used : t
  val unused : t
end

module Internal_param : sig
  val used_required : t
  val unused_required : t
end

module External_param : sig
  val used_required : t
  val unused_required : t
end

module Internal_app : sig
  val externally_used : t
  val internally_used : t
  val unused : t
end
