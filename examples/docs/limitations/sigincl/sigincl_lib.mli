(* sigincl_lib.mli *)
module type T = sig
  type t
  val x : t
end

module M : T

module I : sig
  include T with type t := unit
end
