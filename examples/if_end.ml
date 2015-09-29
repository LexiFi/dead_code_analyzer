let f ?a ?b c = c

let g ?a ?b ?d c = c

let h d = if d then f else g ~d

let () = h false ~a:1 ~b:3 0 |> ignore
