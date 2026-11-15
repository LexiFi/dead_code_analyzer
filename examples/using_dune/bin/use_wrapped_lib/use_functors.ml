open Wrapped_lib.Ctors_fields_in_functor
module Applied = ExternalFunctor (M)

let is_used = ref false

let mark_used () =
  is_used := true
