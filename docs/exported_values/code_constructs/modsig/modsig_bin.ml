(* modsig_bin.ml *)
let () =
  let open Modsig_lib in
  Original.used_directly;
  Alias_without_sig.used_indirectly
