type t = (Lexing.position * Lexing.position) list
    (** Dependencies similar to [cmt_infos.cmt_value_dependencies] in OCaml 5.2 *)

val empty : t (** No signature read *)

type uid_to_decl = Typedtree.item_declaration Shape.Uid.Tbl.t

val init : Cmt_format.cmt_infos -> uid_to_decl option -> (t, string) result
(** [init cmt_infos cmti_infos cmti_uid_to_decl] expects
    [cmt_infos.cmt_annots = Implementation _].
    It reads the [cmt_infos] and the [cmti_uid_to_decl] to retrieve their
    and converts [cmt_infos.cmt_declaration_dependencies] into a single [t].
    It returns an [Ok t] with [t] on success.
    In case the [cmt_infos] does not contain an implementation, it returns an
    [Err msg] with msg a string describing the issue. *)
