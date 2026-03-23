(* inheritance_lib.ml *)
class parent =
  object
    method unused = ()
    method used = ()
    method used_by_child = ()
  end

class child =
  object
    inherit parent
  end
