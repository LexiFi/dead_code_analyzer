open LetIn

let g x y z = match (z, x) with
   (a, b) when b mod 2 <> 0 -> ()
  | _ -> let h a b = a + b in
         let i = f x y ~a:0 z in
         ignore (h i i)
