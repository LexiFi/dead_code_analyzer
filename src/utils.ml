let remove_pp fn =
  let ext = Filename.extension fn in
  let no_ext = Filename.remove_extension fn in
  match Filename.extension no_ext with
  | ".pp" -> Filename.remove_extension no_ext ^ ext
  | _ -> fn

let unit fn =
  Filename.remove_extension (Filename.basename fn)

