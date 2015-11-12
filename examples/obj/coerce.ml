class type ct = object
  method h : unit -> unit
end

class p : ct = object
  method h () = ()
end


class c = object
  inherit p
  method h () = ()
end

let f o = (o :> p)#h()

let () = f (new c)
