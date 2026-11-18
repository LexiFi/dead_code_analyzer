(* functor_lib.mli *)
module M : sig
  type record_externally_used_by_param_requirement = { f : int }
  type record_internally_used_by_param_requirement = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used_by_param_requirement = C
  type constructor_internally_used_by_param_requirement = C
  type constructor_unused = C
end

module External_functor (_ : sig
  type record_externally_used_by_param_requirement = { f : int }
  type constructor_externally_used_by_param_requirement = C
end) : sig
  type record_externally_used = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used = C
  type constructor_unused = C
end
