let o = object
  method f () = ()
  method g () = ()
  method h () = ()
end

let f o = o#f ()

let () = o#g (); f o
