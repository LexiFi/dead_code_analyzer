module M : sig
  val used_by_param_requirement : int
  val used_by_val_requirement : int
  val used_by_val_constraint : int
  val unused : int
end

module type PARAM = sig
  val used_by_param_requirement : int
end

module type VAL_REQ = sig
  val used_by_val_requirement : int
end

module type VAL_CSTR = sig
  val used_by_val_constraint : int
end

val expect_param : (module PARAM) -> unit
