(* opt.ml *)
let map_with_index_on_negative (f : ?index:int -> int -> 'a) l =
  let index = ref 0 in
  let f' x =
    let res =
      if x < 0 then f ~index:(!index) x
      else f x
    in
    incr index;
    res
  in
  List.map f' l

let add_index_to_negative l =
  let add_index ?(index=0) x = x + index in
  map_with_index_on_negative add_index l
