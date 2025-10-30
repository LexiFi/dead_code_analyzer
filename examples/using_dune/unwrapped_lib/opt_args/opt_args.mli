val unused_fun_with_single_never_used_opt_arg : ?never:'a -> unit -> unit
val used_fun_with_single_explicitly_discarded_opt_arg :
  ?never:'a -> unit -> unit
val used_fun_with_single_never_used_opt_arg : ?never:'a -> unit -> unit
val used_fun_with_single_always_used_opt_arg : ?always:'a -> unit -> unit
val used_fun_with_single_sometimes_used_opt_arg :
  ?sometimes:'a -> unit -> unit
val internally_used_fun_with_single_never_used_opt_arg :
  ?never:'a -> unit -> unit
val internally_used_fun_with_single_always_used_opt_arg :
  ?always:'a -> unit -> unit
val internally_used_fun_with_single_sometimes_used_opt_arg :
  ?sometimes:'a -> unit -> unit
val externally_used_fun_with_single_never_used_opt_arg :
  ?never:'a -> unit -> unit
val externally_used_fun_with_single_always_used_opt_arg :
  ?always:'a -> unit -> unit
val externally_used_fun_with_single_sometimes_used_opt_arg :
  ?sometimes:'a -> unit -> unit
val multiple_never_used_opt_args :
  ?never1:'a -> ?never2:'b -> ?never3:'c -> unit -> unit
val multiple_always_used_opt_args :
  ?always1:'a -> ?always2:'b -> ?always3:'c -> unit -> unit
val multiple_sometimes_used_opt_args :
  ?sometimes1:'a -> ?sometimes2:'b -> ?sometimes3:'c -> unit -> unit
val multiple_nas_used_opt_args :
  ?never:'a -> ?always:'b -> ?sometimes:'c -> unit -> unit
val multiple_ans_used_opt_args :
  ?always:'a -> ?never:'b -> ?sometimes:'c -> unit -> unit
val multiple_sna_used_opt_args :
  ?sometimes:'a -> ?never:'b -> ?always:'c -> unit -> unit
val multiple_san_used_opt_args :
  ?sometimes:'a -> ?always:'b -> ?never:'c -> unit -> unit
val multiple_nsa_used_opt_args :
  ?never:'a -> ?sometimes:'b -> ?always:'c -> unit -> unit
val multiple_asn_used_opt_args :
  ?always:'a -> ?sometimes:'b -> ?never:'c -> unit -> unit
