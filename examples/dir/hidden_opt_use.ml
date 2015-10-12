let f ?a ?b x = x

let apply (f: ?a:'a -> ?b:'b -> 'c -> 'd) ?a x = f ?a ~b:0 x

let () = apply f 2 |> ignore
