module M : sig
  type record_externally_used_by_param_requirement = { f : int }
  type record_externally_used_by_val_requirement = { f : int }
  type record_externally_used_by_val_constraint = { f : int }
  type record_internally_used_by_param_requirement = { f : int }
  type record_internally_used_by_val_requirement = { f : int }
  type record_internally_used_by_val_constraint = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used_by_param_requirement =  C
  type constructor_externally_used_by_val_requirement =  C
  type constructor_externally_used_by_val_constraint =  C
  type constructor_internally_used_by_param_requirement =  C
  type constructor_internally_used_by_val_requirement =  C
  type constructor_internally_used_by_val_constraint =  C
  type constructor_unused =  C
end

module type PARAM = sig
  type record_externally_used_by_param_requirement = { f : int }
  type constructor_externally_used_by_param_requirement = C
end

module type VAL_REQ = sig
  type record_externally_used_by_val_requirement = { f : int }
  type constructor_externally_used_by_val_requirement = C
end

module type VAL_CSTR = sig
  type record_externally_used_by_val_constraint = { f : int }
  type constructor_externally_used_by_val_constraint = C
end

val expect_param : (module PARAM) -> unit

