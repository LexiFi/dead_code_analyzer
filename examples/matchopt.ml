let foo ?(x = true) ?(y = 42) ?z () = match z with
    Some n -> n
  | _ -> y

let w = match foo ~x:false () with
    n when n = foo ~x:true ~z:12 () -> n
  | _ when true -> foo ~x:true ()
  | 1 -> foo ()
