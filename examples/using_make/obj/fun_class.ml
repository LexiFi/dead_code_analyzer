class c () = object
  method m1 = ()
  method m2 = ()
  method m3 = ()
  method m4 = ()
end

let f o = o#m1

let () =
  let obj = new c () in
  obj # m2;
  f obj;
  let o = (new c () :> <m4:unit>) in
  ignore (o)
