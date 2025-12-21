let unit fn =
  let u = Filename.remove_extension (Filename.basename fn) in
  match Filename.extension u with
  | ".pp" -> Filename.remove_extension u
  | _ -> u

