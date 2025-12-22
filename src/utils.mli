val unit : string -> string
(** [unit filename] estimates the compilation unit of [filename] *)

val signature_of_modtype :
  ?select_param:bool -> Types.module_type -> Types.signature
(** [signature_of_modtype ?select_param modtype] returns the selected signature
    of [modtype]. If [modtype] is a functor, then [select_param] is used to
    select either the signature of the parameter or the result of the functor.
    Note: [select_param] is [false] by default. If set to [true], it is reset to
          [false] after looking for the parameter of the first functor.
          There is currently no way to select the parameter of a parameter.  *)
