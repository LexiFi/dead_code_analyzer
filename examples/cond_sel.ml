let f ?a ?b c = c

let g ?a ?b c = c

let h ?a ?b c d =
  (if (d >= 0) then f
  else g) ?a ?b c

let f ?a ?b c = c

let g ?a ?b c = c

let h ?a ?b c d = 
  begin match d with
    | 0 -> f
    | _ -> g end
  ?a ?b c
