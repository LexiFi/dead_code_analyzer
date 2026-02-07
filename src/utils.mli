val remove_pp : string -> string
(** [remove_pp filepath] removes the `.pp` extension (if it exists) from
    [filepath]. Eg. [remove_pp "dir/foo.pp.ml" = "dir/foo.ml"] *)

val unit : string -> string
(** [unit filepath] estimates the compilation unit of [filepath] *)

val kind : exclude:(string -> bool) -> string -> [> `Cmi | `Cmt | `Dir | `Ignore ]
(** [kind ~exclude filepath] returns the kind of [filepath].
    If [exclude filepath = true], [filepath] does not exists, or [filepath]
    does not fit in qnother kind, then its kind is [`Ignore].
    Other kinds are self explanatory. *)

module StringSet : Set.S with type elt = String.t
