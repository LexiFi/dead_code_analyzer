module Applied = Functors.ExternalFunctor (Functors.M)

let is_used = ref false

let mark_used () =
  is_used := true
