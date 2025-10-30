open FooCl

class c = object
  inherit p1
  inherit p2
end

let o = new c

let f o = o#f ()

let () = o#g (); f o
