let g ?(c = 0) ?(d = 0) () = c + d

let z = g ~d:30 ()

let t = let tmp ?(d = 12) () = d in g ~c:(tmp ~d:5 ()) ~d:0 ()

let u = g ~d:40 ()
