(* constructor_lib.mli *)
class int_stack :
  int list ->
  object
    method push : int -> unit
    method pop : unit
    method peek : int option
    method reset : unit
  end
