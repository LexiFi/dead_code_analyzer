(* sigincl_lib.ml *)
module type T = sig
  type t
  val x : t
end

module M = struct
  type t = unit
  let x = ()
end

module I = M
