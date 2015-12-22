class c () = object
  method m1 = ()
  method m2 = ()
  method m3 = ()
end

let f o = o#m1

let () =
  let obj = new c () in
  obj # m2;
  f obj
