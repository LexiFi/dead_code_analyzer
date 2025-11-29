(* extenal uses with explicit module *)
let () =
  ignore Wrapped_lib.Without_class.immediate#used_int;
  ignore Wrapped_lib.Without_class.immediate#externally_used_int;
  Wrapped_lib.Without_class.immediate#used_fun ();
  Wrapped_lib.Without_class.immediate#externally_used_fun ()

(* external uses using local open *)
let () =
  let open Wrapped_lib in
  ignore (Without_class.factory ())#used_int;
  let open Without_class in
  (factory ())#used_fun ()

(* external uses using "global" open *)
open Wrapped_lib.Without_class
let () =
  ignore (factory ())#externally_used_int;
  (factory ())#externally_used_fun ()

let is_used = ref false
let mark_used () =
  is_used := true
