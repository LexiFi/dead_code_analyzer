module Filepath : sig

  type t = string

  val remove_pp : t -> t
  (** [remove_pp filepath] removes the `.pp` extension (if it exists) from
      [filepath]. Eg. [remove_pp "dir/foo.pp.ml" = "dir/foo.ml"] *)

  val unit : t -> string
  (** [unit filepath] estimates the compilation unit of [filepath] *)

  type kind =
    | Cmi (** .cmi file *)
    | Cmt (** .cmt file *)
    | Dir (** Directory *)
    | Ignore (** Irrelevant for the analyzer *)

  val kind : exclude:(t -> bool) -> t -> kind
  (** [kind ~exclude filepath] returns the kind of [filepath].
      If [exclude filepath = true], [filepath] does not exists, or [filepath]
      does not fit in another kind, then its kind is [Ignore].
      Other kinds are self explanatory. *)
end

module StringSet : Set.S with type elt = String.t
