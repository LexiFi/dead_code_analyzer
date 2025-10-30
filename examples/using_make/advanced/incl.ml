let f () = ()

include Mod

class d = c

let f () = ignore x |> f

let () =
  let o = new d in
  o#f |> f
