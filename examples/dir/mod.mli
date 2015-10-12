module type G = sig
  val is_even: int -> bool
  val is_odd: int -> bool
end

module I: G

module M(G: G): sig
  module G: G
  val f: int -> bool
  val g: int -> bool
end
