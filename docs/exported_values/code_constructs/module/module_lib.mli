(* module_lib.mli *)
module M : sig
  type t
  val used_directly : t
  val used_indirectly : t
  val unused : t
end
