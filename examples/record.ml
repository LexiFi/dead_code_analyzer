type t =
  {
    mutable f: (?a:int -> ?b:int -> unit -> unit)
  }

let r = {f = fun ?a ?b c -> c}

let () = r.f ~a:0 ()
