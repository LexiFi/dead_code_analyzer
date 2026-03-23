(* object_type_lib.mli *)
type int_stack =
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >

val int_stack : int_stack
