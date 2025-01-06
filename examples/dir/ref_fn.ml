let f ?a ?b ?c () = 312

let () = RefFn.r := f ~c:0

let g = f ~c:0

let () =
  let a = f ~c:1 ()
  in RefFn.r := g; print_int (a - (!RefFn.r ~b:1 ()))

