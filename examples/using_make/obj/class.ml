class c = object
  method f () = ()
  method g () = ()
  method h () = ()
end

let o = new c

let f o = o#f ()

let () = o#g (); f o

