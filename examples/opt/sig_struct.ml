include (struct
  let f ?(x = 0) ~y () = ignore (x+y)

  let () = f ~y:1 ()
end : sig
  val f : ?x:int -> y:int -> unit -> unit
end)


let () =
  let g = f ~y:1 in
  g ~x:1 (f ~y:0 ~x:0 ())
