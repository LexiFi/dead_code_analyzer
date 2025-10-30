module M = struct
  let f x = x mod 2 = 0
  let none () = ()
end

module F(M:sig val f: int -> bool end) = struct
  let g x = not @@ M.f x
end

module N = F(M)

let () = ignore @@ N.g 12

module X = F(struct let f _ = false end)

let () = ignore @@ X.g 14

module Unused = Set.Make(struct
    type t = int
    let compare a b = if a > b then 0 else 12
    let none () = ()
  end)
