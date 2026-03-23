(* object_type_lib.ml *)
type int_stack =
  < push : int -> unit
  ; pop : unit
  ; peek : int option
  ; reset : unit
  >

let int_stack =
  object
    val mutable l : int list = []
    method push x = l <- x::l
    method pop =
      match l with
      | [] -> ()
      | _::tl -> l <- tl
    method peek =
      match l with
      | [] -> None
      | hd::_ -> Some hd
    method reset = l <- []
  end
