let f ?a ?b c = c

let m ?b = function
    _::_ as l ->
      let () = f ~a:1 ?b 0 |> ignore in [f ~a:1 ?b 0]
  | [] -> [f ~a:1 0]
