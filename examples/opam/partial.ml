let f ?(a = 1) ?(b = 1) c = ignore c

let () = f ~a:1 ~b:1 0

let g = f ~a:1

let () = g 0
