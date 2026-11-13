module Applied = Wrapped_lib.Functors.ExternalFunctor (Wrapped_lib.Functors.M)

let is_used = ref false

let mark_used () =
  is_used := true
