(* polymorphic_class_lib.mli *)
class ['a] stack :
  object
    method push : 'a -> unit
    method pop : unit
    method peek : 'a option
    method reset : unit
  end
