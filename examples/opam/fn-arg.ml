let f ?a ?b c = c

let g ?b f x = f ?b x

let () = ignore (g (f ~a:1) 0)

let h ?a ?b c = c

let i ?c f x= f ?b:c x

let j ?b f x = f ?b x

let () = ignore (i (h ~a:1) ~c:5 0)

let () = ignore (j (h ~a:1) ~b:3 0)
