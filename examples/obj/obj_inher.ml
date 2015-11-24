class c = object
  method f = ()
  method g = ()
  method h = ()
end

let o : c = object
  inherit c
  method g = ()
end

let o4 = object
  inherit c
  method g = ()
end

let o2 = new c

let o3 = object
  inherit c
  method k = ()
end

let f () = object
  inherit c
  method g = ()
end

let () =
  (f o4#g)#g;
  o3#f;
  o2#f;
  o#g
