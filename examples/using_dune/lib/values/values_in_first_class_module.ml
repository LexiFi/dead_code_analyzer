module M = struct
  let used_by_param_requirement = 42
  let used_by_val_requirement = 42
  let used_by_val_constraint = 42
  let unused = 42
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

let expect_param (module Param : PARAM) = ()
