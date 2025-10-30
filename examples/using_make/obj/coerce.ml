class type ct = object
  method f : unit
  method h : unit -> unit
end

class p : ct = object
  method f = ()
  method h () = ()
end


class c = object
  inherit p
  method! f = ()
  method h () = ()
end

let f o = (o :> p)#h()

let () =
  f (new c);
  let o = (new c :> p) in
  o#f
