(* functor_lib.ml *)
module M = struct
  type record_externally_used_by_param_requirement = { f : int }
  type record_internally_used_by_param_requirement = { f : int }
  type record_unused = { f : int }

  type constructor_externally_used_by_param_requirement = C
  type constructor_internally_used_by_param_requirement = C
  type constructor_unused = C
end

module InternalFunctor (_ : sig
  type record_internally_used_by_param_requirement = { f : int }
  type constructor_internally_used_by_param_requirement = C
end) = struct
end

module ExternalFunctor (_ : sig
  type record_externally_used_by_param_requirement = { f : int }
  type constructor_externally_used_by_param_requirement = C
end) = struct
end

module Applied = InternalFunctor (M)
