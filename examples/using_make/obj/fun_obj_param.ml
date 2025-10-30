let f x = object
  method x = x
  method m = ()
  method n = ()
end

let app_m f = (f ()) # m

let () =
  ignore (((fun () -> f 0) ()) # x);
  app_m (fun () -> f 0)
