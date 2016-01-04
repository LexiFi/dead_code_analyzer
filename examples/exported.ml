module type T = sig
  val five : int
end


module M1 : T = struct
  let five = 5 (* ignored during analysis because M1 does not give explicit sig *)
end


module F (M : T) = struct
  let plus_five n = n + M.five
  let times_five n = n * M.five
end


module M2 : sig
  val five : int
  val plus_five : int -> int
  val ten : int
end = struct
  include M1
  include F(M1)
  let ten = plus_five 5
end


let () =
  let twenty = M2.plus_five 5 + M2.plus_five 5 in
  print_int twenty
