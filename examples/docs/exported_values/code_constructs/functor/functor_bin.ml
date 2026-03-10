(* functor_bin.ml *)
open Functor_lib

module External_app = F(External_param)

let () =
  ignore Internal_app.externally_used;
  ignore External_app.externally_used
