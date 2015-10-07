module M: sig
  val f: int -> bool
  val none: unit -> unit
end

module F(M: sig val f: int -> bool end):sig
  val g: int -> bool
end

module N: sig
  val g: int -> bool
end

module Unused: Set.S
