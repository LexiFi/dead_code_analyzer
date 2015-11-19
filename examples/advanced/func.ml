module type T = sig
  type t
  class c : object
    method f : t
  end
end

module M ( ) : (T with type t = unit) = struct
  type t = unit
  class c = object
    method f = ()
  end
end
