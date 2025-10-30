let f ?a ?b c = c

let () = (f ~a:0) ~b:0 0 |> ignore
