(* class_lib.mli *)
class int_stack :
  object
    method push : int -> unit
    method pop : unit
    method peek : int option
    method reset : unit
  end
