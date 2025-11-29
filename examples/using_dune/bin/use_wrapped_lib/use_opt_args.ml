(* extenal uses with explicit module *)
let () =
  Wrapped_lib.Opt_args.used_fun_with_single_never_used_opt_arg ();
  Wrapped_lib.Opt_args.used_fun_with_single_explicitly_discarded_opt_arg ?never:None ();
  Wrapped_lib.Opt_args.used_fun_with_single_always_used_opt_arg ~always:() ();
  Wrapped_lib.Opt_args.used_fun_with_single_sometimes_used_opt_arg ();
  Wrapped_lib.Opt_args.used_fun_with_single_sometimes_used_opt_arg ~sometimes:() ();
  ()

(* external uses using local open *)
let () =
  let open Wrapped_lib in
  Opt_args.externally_used_fun_with_single_never_used_opt_arg ();
  Opt_args.externally_used_fun_with_single_always_used_opt_arg ~always:() ();
  Opt_args.externally_used_fun_with_single_sometimes_used_opt_arg ();
  Opt_args.externally_used_fun_with_single_sometimes_used_opt_arg ~sometimes:() ();
  let open Opt_args in
  multiple_never_used_opt_args ();
  multiple_always_used_opt_args ~always1:() ~always2:() ~always3:() ();
  multiple_sometimes_used_opt_args ~sometimes1:() ~sometimes2:() ();
  multiple_sometimes_used_opt_args ~sometimes1:() ~sometimes3:() ();
  multiple_sometimes_used_opt_args ~sometimes2:() ~sometimes3:() ();
  ()

(* external uses using "global" open *)
open Wrapped_lib.Opt_args
let () =
  multiple_nas_used_opt_args ~always:() ~sometimes:() ();
  multiple_nas_used_opt_args ~always:() ();
  multiple_ans_used_opt_args ~always:() ~sometimes:() ();
  multiple_ans_used_opt_args ~always:() ();
  multiple_sna_used_opt_args ~always:() ~sometimes:() ();
  multiple_sna_used_opt_args ~always:() ();
  multiple_san_used_opt_args ~always:() ~sometimes:() ();
  multiple_san_used_opt_args ~always:() ();
  multiple_nsa_used_opt_args ~always:() ~sometimes:() ();
  multiple_nsa_used_opt_args ~always:() ();
  multiple_asn_used_opt_args ~always:() ~sometimes:() ();
  multiple_asn_used_opt_args ~always:() ();
  ()

let is_used = ref false
let mark_used () =
  is_used := true
