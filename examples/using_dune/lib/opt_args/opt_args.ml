let unused_fun_with_single_never_used_opt_arg ?never () =
  ignore never

let used_fun_with_single_explicitly_discarded_opt_arg ?never () =
  ignore never

let unexported_fun_with_single_never_used_opt_arg ?never () =
  ignore never

let unexported_fun_with_single_always_used_opt_arg ?always () =
  ignore always

let unexported_fun_with_single_sometimes_used_opt_arg ?sometimes () =
  ignore sometimes

let used_fun_with_single_never_used_opt_arg ?never () =
  ignore never

let used_fun_with_single_always_used_opt_arg ?always () =
  ignore always

let used_fun_with_single_sometimes_used_opt_arg ?sometimes () =
  ignore sometimes

let internally_used_fun_with_single_never_used_opt_arg ?never () =
  ignore never

let internally_used_fun_with_single_always_used_opt_arg ?always () =
  ignore always

let internally_used_fun_with_single_sometimes_used_opt_arg ?sometimes () =
  ignore sometimes

let externally_used_fun_with_single_never_used_opt_arg ?never () =
  ignore never

let externally_used_fun_with_single_always_used_opt_arg ?always () =
  ignore always

let externally_used_fun_with_single_sometimes_used_opt_arg ?sometimes () =
  ignore sometimes

let multiple_never_used_opt_args ?never1 ?never2 ?never3 () =
  ignore never1;
  ignore never2;
  ignore never3

let multiple_always_used_opt_args ?always1 ?always2 ?always3 () =
  ignore always1;
  ignore always2;
  ignore always3

let multiple_sometimes_used_opt_args ?sometimes1 ?sometimes2 ?sometimes3 () =
  ignore sometimes1;
  ignore sometimes2;
  ignore sometimes3

let multiple_nas_used_opt_args ?never ?always ?sometimes () =
  ignore never;
  ignore always;
  ignore sometimes

let multiple_ans_used_opt_args ?always ?never ?sometimes () =
  ignore never;
  ignore always;
  ignore sometimes

let multiple_sna_used_opt_args ?sometimes ?never ?always () =
  ignore never;
  ignore always;
  ignore sometimes

let multiple_san_used_opt_args ?sometimes ?always ?never () =
  ignore never;
  ignore always;
  ignore sometimes

let multiple_nsa_used_opt_args ?never ?sometimes ?always () =
  ignore never;
  ignore always;
  ignore sometimes

let multiple_asn_used_opt_args ?always ?sometimes ?never () =
  ignore never;
  ignore always;
  ignore sometimes

let () = (* single tests *)
  unexported_fun_with_single_never_used_opt_arg ();
  unexported_fun_with_single_always_used_opt_arg ~always:() ();
  unexported_fun_with_single_sometimes_used_opt_arg ();
  unexported_fun_with_single_sometimes_used_opt_arg ~sometimes:() ();
  used_fun_with_single_never_used_opt_arg ();
  used_fun_with_single_explicitly_discarded_opt_arg ?never:None ();
  used_fun_with_single_always_used_opt_arg ~always:() ();
  used_fun_with_single_sometimes_used_opt_arg ();
  used_fun_with_single_sometimes_used_opt_arg ~sometimes:() ();
  internally_used_fun_with_single_never_used_opt_arg ();
  internally_used_fun_with_single_always_used_opt_arg ~always:() ();
  internally_used_fun_with_single_sometimes_used_opt_arg ();
  internally_used_fun_with_single_sometimes_used_opt_arg ~sometimes:() ();
  ()

let () = (* multiple and positional tests *)
  multiple_never_used_opt_args ();
  multiple_always_used_opt_args ~always1:() ~always2:() ~always3:() ();
  multiple_sometimes_used_opt_args ~sometimes1:() ~sometimes2:() ();
  multiple_sometimes_used_opt_args ~sometimes1:() ~sometimes3:() ();
  multiple_sometimes_used_opt_args ~sometimes2:() ~sometimes3:() ();
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
