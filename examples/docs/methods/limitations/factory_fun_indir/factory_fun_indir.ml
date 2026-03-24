(* factoy_fun_indir.ml *)
let factory_with_intermediate_binding () =
  let res =
    object method unused_method = () end
  in
  res

let random_factory () =
  if Random.bool () then
    object method m = () end
  else
    let res = object method m = () end in
    res
