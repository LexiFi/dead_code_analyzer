(* polymorphic_type_lib.mli *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) both = {left : 'a; right : 'b}
