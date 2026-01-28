open Wrapped_lib

(* use Imt_include *)
let () =
  ignore Imt_include.used;
  ignore Imt_include.externally_used;
  ignore Imt_include.sometimes_used

(* use Imt_include_use_less *)
let () =
  ignore Imt_include_use_less.used;
  ignore Imt_include_use_less.externally_used

(* use Imt_internal_include *)
let () =
  ignore Imt_internal_include.used;
  ignore Imt_internal_include.externally_used

let is_used = ref false
let mark_used () =
  is_used := true
