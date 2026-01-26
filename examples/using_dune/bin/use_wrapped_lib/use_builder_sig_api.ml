open Wrapped_lib

(* use Bsa *)
let () =
  ignore Bsa.Parameter.used;
  ignore Bsa.used;
  ignore Bsa.externally_used;
  ignore Bsa.sometimes_used

(* use Bsa_less *)
let () =
  ignore Bsa_use_less.used;
  ignore Bsa_use_less.externally_used

let is_used = ref false
let mark_used () =
  is_used := true
