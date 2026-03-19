(* immediate_object_lib.mli *)
val int_stack :
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >
