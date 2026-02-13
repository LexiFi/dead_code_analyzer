let remove_pp fn =
  let ext = Filename.extension fn in
  let no_ext = Filename.remove_extension fn in
  match Filename.extension no_ext with
  | ".pp" -> Filename.remove_extension no_ext ^ ext
  | _ -> fn

let unit fn =
  Filename.remove_extension (Filename.basename fn)

let rec signature_of_modtype ?(select_param = false) modtype =
  let open Types in
  match modtype with
  | Mty_signature sg -> sg
  | Mty_functor (_, t) when not select_param -> signature_of_modtype t
  | Mty_functor (Named (_, t), _) -> signature_of_modtype t
  | _ -> []
