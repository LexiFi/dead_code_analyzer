(* foo's and bar's a are always used; b never is *)
let foo ?a ?b () = 0
let bar ?a = foo ~a
let baz = foo

let x = bar ~a:0 ()
let y = baz ~a:0 ()


(* foo's a and b are never used; bar's a is always used *)
let foo ?a ?b () = 0
let bar ?a = foo

let x = bar ~a:0 ()
