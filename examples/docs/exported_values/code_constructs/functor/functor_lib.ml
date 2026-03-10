(* functor_lib.ml *)
type t = unit

module F (P : sig
  val used_required : t
  val unused_required : t
end) = struct
  let externally_used = P.used_required
  let internally_used = P.used_required
  let unused = P.used_required
  let unused_unexported = P.used_required
  let () = internally_used
end

module Internal_param = struct
  let used_required = ()
  let unused_required = ()
end

module External_param = struct
  let used_required = ()
  let unused_required = ()
end

module Internal_app = F(Internal_param)
