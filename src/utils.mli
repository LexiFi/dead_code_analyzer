module Filepath : sig

  type t = string

  val remove_pp : t -> t
  (** [remove_pp filepath] removes the `.pp` extension (if it exists) from
      [filepath]. Eg. [remove_pp "dir/foo.pp.ml" = "dir/foo.ml"] *)

  val unit : t -> string
  (** [unit filepath] estimates the compilation unit of [filepath] *)

  type kind =
    | Cmti (** .cmti file *)
    | Cmt (** .cmt file *)
    | Dir (** Directory *)
    | Ignore (** Irrelevant for the analyzer *)

  val kind : exclude:(t -> bool) -> t -> kind
  (** [kind ~exclude filepath] returns the kind of [filepath].
      If [exclude filepath = true], [filepath] does not exists, or [filepath]
      does not fit in another kind, then its kind is [Ignore].
      Other kinds are self explanatory. *)
end

val signature_of_modtype :
  ?select_param:bool -> Types.module_type -> Types.signature
(** [signature_of_modtype ?select_param modtype] returns the selected signature
    of [modtype]. If [modtype] is a functor, then [select_param] is used to
    select either the signature of the parameter or the result of the functor.
    Note: [select_param] is [false] by default. If set to [true], it is reset to
          [false] after looking for the parameter of the first functor.
          There is currently no way to select the parameter of a parameter.  *)

module StringSet : Set.S with type elt = String.t
