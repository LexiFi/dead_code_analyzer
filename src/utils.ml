let unit fn =
  let u = Filename.remove_extension (Filename.basename fn) in
  match Filename.extension u with
  | ".pp" -> Filename.remove_extension u
  | _ -> u

let rec signature_of_modtype ?(select_param = false) modtype =
  let open Types in
  match modtype with
  | Mty_signature sg -> sg
  | Mty_functor (_, t) when not select_param -> signature_of_modtype t
  | Mty_functor (Named (_, t), _) -> signature_of_modtype t
  | _ -> []
