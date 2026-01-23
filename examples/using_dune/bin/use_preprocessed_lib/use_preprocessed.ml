open Preprocessed_lib.Preprocessed

let () = (* use values *)
  ignore used;
  ignore externally_used

let () = (* use methods *)
  ignore immediate#used;
  ignore immediate#externally_used

let () = (* use constructors *)
  ignore Used;
  ignore Externally_used

let () = (* use record fields *)
  let r = {unused = 42; used = 42; internally_used = 42; externally_used = 42} in
  ignore r.used;
  ignore r.externally_used

let () = (* use optional arguments *)
  exported_f ~always:42 ~externally:42 ();
  externally_used_f ~always:42 ~sometimes:42 ();
  externally_used_f ~always:42 ()

let is_used = ref false
let mark_used () =
  is_used := true
