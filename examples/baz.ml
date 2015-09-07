let g ?(c = 0) ?(d = 0) () = c + d

let z = g ~d:30 ()

let t = g ~c:40 ()
