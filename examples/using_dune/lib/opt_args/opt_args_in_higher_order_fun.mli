val partially_applied_with_never_used_opt_arg :
  ?never:'a -> unit -> unit -> unit
val partially_applied_with_always_used_opt_arg :
  ?always:'a -> unit -> unit -> unit
val partially_applied_with_sometimes_used_opt_arg :
  ?sometimes:'a -> unit -> unit -> unit
val shadowing_never_used_arg : ?duplicate:'a -> ?duplicate:'b -> unit -> unit
val shadowing_always_used_arg :
  ?duplicate:'a -> ?duplicate:'b -> unit -> unit
val expect_fun_with_never_used_opt_arg : (?type_used:'a -> unit -> 'b) -> 'b
val expect_fun_with_always_used_opt_arg :
  (?type_used:unit -> unit -> 'a) -> 'a
val expect_fun_with_never_used_opt_arg_with_sig :
  (?type_used:'a -> unit -> unit) -> unit
val expect_fun_with_always_used_opt_arg_with_sig :
  (?type_used:unit -> unit -> unit) -> unit
val fun_with_always_type_used_opt_arg : ?type_used:'a -> unit -> unit
