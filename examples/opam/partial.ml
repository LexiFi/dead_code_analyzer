let f ?(a = 1) ?(b = 1) ?(c = 1) d = ignore d

let () = f ~a:1 ~b:1 0

let g = f ~a:1

let () = g ~b:1 0
