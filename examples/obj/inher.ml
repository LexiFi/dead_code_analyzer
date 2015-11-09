class p = object
  method f () = ()
  method g () = ()
  method h () = ()
end

class c = object
  inherit p
end

let o = new c

let f o = o#f ()

let () = o#g (); f o

