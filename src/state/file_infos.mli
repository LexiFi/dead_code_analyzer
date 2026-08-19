(** Information about an analyzable file ([.cmti] or [.cmt] file) *)

(** Data specific to the file type (.cmt or .cmti) *)
type cm_infos =
  | Cmti of {
        sign : Typedtree.signature;
        cmti_uid_to_decl : Location_dependencies.uid_to_decl;
          (** Extracted from [cmt_infos.cmt_uid_to_decl] *)
      } (* only a .cmti was read *)
  | Cmt of {
        strc : Typedtree.structure;
        sign : Typedtree.signature option;
          (** signature read in the corresponding .cmti (if available) *)
        location_dependencies : Location_dependencies.t;
          (** Dependencies similar to [cmt_infos.cmt_value_dependencies]
              in OCaml 5.2. *)
      } (* infos coming from a .cmt *)
  | Neither (* no file read or the content was discarded *)

type t = {
  builddir : string; (** The [cmt_builddir] *)
  cm_file : string; (** The filepath currently analyzed *)
  cm_infos : cm_infos;
    (** Data specific to the file type (.cmt or .cmti) *)
  modname : string; (** Either [cmti_name] or [cmt_modname] *)
  sourcepath : string option; (** The path to the associated source file *)
}

val empty : t (** No file info *)

val init :
  comp_unit_to_path: (string, string) Hashtbl.t
  -> string
  -> (t, string) result
(** [init cm_file] expects either a [.cmti] or [.cmt] filepath as argument and
    returns an [Ok t] with [t] filled using the [cmtit_file].
    In case the file does not exist, it cannot be read, or its extension is
    invalid, then it returns an [Err msg] with msg a string
    describing the issue.
    [comp_unit_to_path] is used to load external cm files if necessary. *)

val change_file :
  comp_unit_to_path: (string, string) Hashtbl.t
  -> t
  -> string
  -> (t, string) result
(** [change_file t cm_file] expects either a [.cmti] or a [.cmt] filepath as
    argument. [cm_file] must be the same as [t.cm_file], ignoring the
    extension.
    The returned value is either a simple update of [t] if the necessary
    [cmti_infos] or [cmt_infos] is available. Otherwise, it is the result of
    [init t].
    [comp_unit_to_path] is used to load external cm files if necessary. *)

val has_sourcepath : t -> bool

val get_builddir : t -> string
val get_sourcepath : t -> string
val get_sourceunit : t -> string
val get_modname : t -> string
