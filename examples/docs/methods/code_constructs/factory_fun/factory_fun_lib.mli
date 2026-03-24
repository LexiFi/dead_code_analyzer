(* factoy_fun_lib.mli *)
val get_stack :
  unit ->
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >
