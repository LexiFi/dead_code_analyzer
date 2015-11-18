let f () = ()
let x = 2

class p = object
  method f = ()
end

class c = object
  inherit p
end
