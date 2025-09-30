let f ?(a = 0) ?(b = 0) () = a + b

let x = f ~b:30 ()

let y = f ~b:40 ()
