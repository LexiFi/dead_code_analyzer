class p1 = object(p)
  method f () = ()
  method g () = p#f ()
end

class p2 = object
  method f () = ()
end

class c = object(self)
  inherit p1 as super1
  inherit p2 as super2
  method f () = super1#f (super2#f ())
  method g () = super1#g ()
end
