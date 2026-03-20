(* class_type_lib.mli *)
class type int_stack =
  object
    method push : int -> unit
    method pop : unit
    method peek : int option
    method reset : unit
  end

val int_stack : int_stack
