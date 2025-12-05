(** Information about a analyzable file ([.cmti] or [.cmt] file) *)

type t = {
  cm_file : string; (** The filepath currently analyzed *)
  sourcepath : string option; (** The path to the associated source file *)
  builddir : string; (** The [cmt_builddir] *)
  modname : string; (** Either [cmti_name] or [cmt_modname] *)
  cmi_infos : Cmi_format.cmi_infos option;
  cmt_infos : Cmt_format.cmt_infos option;
  cmti_infos : Cmt_format.cmt_infos option;
}

val empty : t (** No file info *)

val init : string -> (t, string) result
(** [init cm_file] expects either a [.cmti] or [.cmt] filepath as argument and
    returns an [Ok t] with [t] filled using the [cmtit_file].
    In case the file does not exist, it cannot be read, or its extension is
    invalid, then it returns an [Err msg] with msg a string
    describing the issue. *)

val change_file : t -> string -> (t, string) result
(** [change_file t cm_file] expects either a [.cmti] or a [.cmt] filepath as
    argument. [cm_file] must be the same as [t.cm_file], ignoring the
    extension.
    The returned value is either a simple update of [t] if the necessary
    [cmti_infos] or [cmt_infos] is available. Otherwise, it is the result of
    [init t] *)

val has_sourcepath : t -> bool

val get_builddir : t -> string
val get_sourcepath : t -> string
val get_sourceunit : t -> string
val get_modname : t -> string
