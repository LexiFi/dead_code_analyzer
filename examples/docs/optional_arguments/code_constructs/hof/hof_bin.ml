(* hof_bin.ml *)
let map_with_index (f : ?index:int -> int -> 'a) l =
  let f' index = f ~index in
  List.mapi f' l

let add_index ?(index = 0) x =
  x + index

let add_index l =
  map_with_index add_index l
