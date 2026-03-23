(* inheritance_bin.ml *)
let () =
  let p = new Inheritance_lib.parent in
  let c = new Inheritance_lib.child in
  p#used;
  c#used_by_child
