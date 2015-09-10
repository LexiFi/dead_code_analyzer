let foo ?a ?b () = 
  let bar ?(a = 0) ?(b = 0) () = a + b 
  in bar ()

let baz ?(b = 0) ?(a = 0) () = if a <> 0 then foo ~a () else foo ~a ~b ()

let x = baz ~a:12
