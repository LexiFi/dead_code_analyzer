class type ct = object
  method f : unit -> unit
  method g : unit -> unit
  method h : unit -> unit
end

class p : ct = object(self)
  method f () = ()
  method g () = self#f()
  method h () = self#g()
end

class c = object
  inherit p as super
  method h () = super#g (super#h())
end
