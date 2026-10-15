module M = struct
  let externally_used_by_param_requirement = 42
  let externally_used_by_val_requirement = 42
  let externally_used_by_val_constraint = 42
  let internally_used_by_param_requirement = 42
  let internally_used_by_val_requirement = 42
  let internally_used_by_val_constraint = 42
  let unused = 42
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

let expect_param (module Param : PARAM) = ()

module type INTERNAL_PARAM = sig
  val internally_used_by_param_requirement : int
end

module type INTERNAL_VAL_REQ = sig
  val internally_used_by_val_requirement : int
end

module type INTERNAL_VAL_CSTR = sig
  val internally_used_by_val_constraint : int
end

let () =
  let _ = (module M : INTERNAL_VAL_CSTR) in
  let _ : (module INTERNAL_VAL_REQ) = (module M) in
  let expect_param (module Param: INTERNAL_PARAM) = () in
  expect_param (module M)
