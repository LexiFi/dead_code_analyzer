class p1 = object
  method f () = ()
  method g () = ()
  method h () = ()
end

class p2 = object
  method g () = ()
  method h () = ()
end

class c = object
  method f () = ()
  inherit p1
  inherit p2
  method g () = ()
end

let o = new c

let f o = o#f ()

let () = o#g (); f o
