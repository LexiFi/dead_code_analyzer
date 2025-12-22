(** Information about a analyzable file ([.cmti] or [.cmt] file) *)

type t = {
  builddir : string; (** The [cmt_builddir] *)
  cm_file : string; (** The filepath currently analyzed *)
  cmi_sign : Types.signature option; (** Extracted from [cmi_infos] *)
  cmt_struct : Typedtree.structure option;
    (** Extracted from a cmt's [cmt_infos.cmt_annots] *)
  cmti_uid_to_decl : Location_dependencies.uid_to_decl option;
    (** Extracted from a cmti's [cmt_infos] *)
  location_dependencies : Location_dependencies.t;
    (** Dependencies similar to [cmt_infos.cmt_value_dependencies] in OCaml 5.2 *)
  modname : string; (** Either [cmti_name] or [cmt_modname] *)
  sourcepath : string option; (** The path to the associated source file *)
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
