(* foo's and bar's a are always used; b never is *)
let foo ?a ?b () = 0
let bar ?a = foo ~a

let x = bar ~a:0 ()


(* foo's a and b never are used; bar's a always is used *)
let foo ?a ?b () = 0
let bar ?a = foo

let x = bar ~a:0 ()
