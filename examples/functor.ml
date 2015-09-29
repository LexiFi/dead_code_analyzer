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
