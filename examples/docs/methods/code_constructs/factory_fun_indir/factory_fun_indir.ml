(* factoy_fun_indir.ml *)
let factory_with_intermediate_binding () =
  let res =
    object
      method used_locally = ()
      method used = ()
      method unused = ()
    end
  in
  res#used_locally;
  res

let random_factory () =
  if Random.bool () then
    object
      method used = ()
      method unused = ()
    end
  else begin
    let res =
      object
        method used = ()
        method unused = ()
      end
    in
    res
  end

let () =
  (factory_with_intermediate_binding ()) # used;
  (random_factory ()) # used
