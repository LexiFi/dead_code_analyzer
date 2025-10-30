module M1 = struct
  let id x = x
end

module M2 = M1

let id = M2.id
