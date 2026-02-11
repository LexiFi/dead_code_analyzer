(* module_lib.mli *)
module M : sig
  type t
  val externally_used : t
  val internally_used : t
  val unused : t
end
