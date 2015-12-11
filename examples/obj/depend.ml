module type T = sig
  class c : object
    method f : unit
  end
end

module M = struct
  class c = object
    method f = ()
  end
end

include (M : T)

class alias = c

let () = (new alias) # f
