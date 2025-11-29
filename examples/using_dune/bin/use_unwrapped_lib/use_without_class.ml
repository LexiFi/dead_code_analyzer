(* extenal uses with explicit module *)
let () =
  ignore Without_class.immediate#used_int;
  ignore Without_class.immediate#externally_used_int;
  Without_class.immediate#used_fun ();
  Without_class.immediate#externally_used_fun ()

(* external uses using local open *)
let () =
  let open Without_class in
  ignore (factory ())#used_int;
  (factory ())#used_fun ()

(* external uses using "global" open *)
open Without_class
let () =
  ignore (factory ())#externally_used_int;
  (factory ())#externally_used_fun ()

let is_used = ref false
let mark_used () =
  is_used := true
