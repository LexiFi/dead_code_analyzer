(* functor_bin.ml *)
open Functor_lib

module ExternalApp = F(ExternalParam)

let () =
  ignore InternalApp.externally_used;
  ignore ExternalApp.externally_used
