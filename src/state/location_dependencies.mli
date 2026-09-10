type t = (Lexing.position * Lexing.position) list
    (** Dependencies similar to [cmt_infos.cmt_value_dependencies] in OCaml 5.2 *)

val empty : t (** No signature read *)

#if OCAML_VERSION >= (5, 3, 0)
type uid_to_decl = Typedtree.item_declaration Shape.Uid.Tbl.t
#else
(* The corresponding [cmt_infos.cmt_uid_to_decl] is introduced in
   OCaml 5.2 but not used until OCaml 5.3 (see {!init} below).
   We still provide a constructor to keep the state representation as
   uniform as possible.
*)
type uid_to_decl = NA
#endif

val init :
  comp_unit_to_path: (string, string) Hashtbl.t
  -> Cmt_format.cmt_infos
  -> uid_to_decl option
  -> (t, string) result
(** [init ~comp_unit_to_path cmt_infos cmti_uid_to_decl] expects
    [cmt_infos.cmt_annots = Implementation _].
    It returns an [Ok t] with [t] on success.
    In case the [cmt_infos] does not contain an implementation, it returns
    an [Err msg] with msg a string describing the issue.

    In OCaml <= 5.2, it simply maps [cmt_infos.cmt_value_dependencies] to
    the correpsonding locations. [comp_unit_to_path] and [cmti_uid_to_decl]
    are unused.

    In OCaml >= 5.3, it reads [cmt_infos.cmt_uid_to_decl] and
    [cmti_uid_to_decl] to convert [cmt_infos.cmt_declaration_dependencies]
    into a single [t] (equivalent to the result provided in OCaml <= 5.2
    using [cmt_infos.cmt_value_dependencies]).
    [comp_unit_to_path] is used to load external .cmt and .cmti files if
    necessary.
*)
