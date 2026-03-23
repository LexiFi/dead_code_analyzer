(* inheritance_lib.mli *)
class parent :
  object
    method unused : unit
    method used : unit
    method used_by_child : unit
  end

class child :
  object
    inherit parent
  end
