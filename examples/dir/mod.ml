module type G = sig
  val is_even: int -> bool
  val is_odd: int -> bool
end

module I: G = struct
  let is_even x = x mod 2 = 0
  let is_odd x = not @@ is_even x
end


module M(G: G) = struct
  module G = G

  let f = G.is_even
  let g = G.is_odd
end

module MG = M(I)

let test = MG.f 12
