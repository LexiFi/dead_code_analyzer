(* inline_record_lib.mli *)
type ('a, 'b) t =
  | Both of {left : 'a; right : 'b}
  | Left of 'a
  | Right of 'b

val get_left_opt : ('a, 'b) t -> 'a option
