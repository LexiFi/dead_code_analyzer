let f x = object
  method x = x
end

let o f = f 12

let () = ignore ((o f) # x)
