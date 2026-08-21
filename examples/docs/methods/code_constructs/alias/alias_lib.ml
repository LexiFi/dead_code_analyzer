(* alias_lib.ml *)
let original =
  object
    method used = ()
    method used_by_alias = ()
    method unused = ()
  end

let alias = original
