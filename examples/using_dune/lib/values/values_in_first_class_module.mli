module M : sig
  val externally_used_by_param_requirement : int
  val externally_used_by_val_requirement : int
  val externally_used_by_val_constraint : int
  val internally_used_by_param_requirement : int
  val internally_used_by_val_requirement : int
  val internally_used_by_val_constraint : int
  val unused : int
end

module type PARAM = sig
  val externally_used_by_param_requirement : int
end

module type VAL_REQ = sig
  val externally_used_by_val_requirement : int
end

module type VAL_CSTR = sig
  val externally_used_by_val_constraint : int
end

val expect_param : (module PARAM) -> unit
