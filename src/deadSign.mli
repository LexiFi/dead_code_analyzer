val collect_export_from_signature :
  path: string list ->
  comp_unit: string ->
  Typedtree.signature
  -> unit
(** [collect_export_from_signature ~path ~comp_unit signature]
    recursively traverse the [signature] items and store exported elements
    (values, constructors, ...) in {!DeadCommon.decs} (or equivalent).
    - [path] is the current module path. E.g. for a value in a submodule,
      the path is [Module.Submodule].
    - [comp_unit] is the compilation unit of the [sig_item].
*)

val collect_export_from_structure :
  path: string list ->
  comp_unit: string ->
  Typedtree.structure
  -> unit
(** [collect_export_from_structure ~path ~comp_unit structure]
    recursively traverse the [structure] items and store exported elements
    (values, constructors, ...) in {!DeadCommon.decs} (or equivalent).
    See {!collect_export_from_signature} above for more information.
*)

val collect_from_include :
  Typedtree.include_declaration
  -> unit
(** [collect_from_include incl_decl]
    store exported object, class elements and type equivalences found in
    [incl_decl].
*)

val collect_eq_from_module_alias :
  path: string list ->
  Typedtree.module_binding
  -> unit
(** [collect_eq_from_module_alias ~path module_binding]
    links exports in the [module_binding]'s expr to the new module if it is
    an alias (M1 = M2). Aliases on functor applications are included.
    [path] is the current module id's path (with the new id included) in
    reverse order (i.e. the new id is at the head).
*)

val correct_export : Types.signature_item -> unit
(** Recursively traverse the sig_item, unexport its content and disables
    the reporting of optional arguments on its values. This is used to
    correct optimistic exports of {!collect_export_from_structure} above,
    in the presence of module signature disagreements (see {!modtype} below).
*)

val modtype :
  on_mismatch: (Types.signature -> unit) ->
  Typedtree.module_type
  -> unit
(** [modtype ~on_mismatch mt] checks that the Typedtree and Types signatures
    for [mt] agree. I.e. if the first one is explicit when the second one is.
    If not, then the first one is implicit and [on_mismatch] is called on the
    second. *)

val eof : unit -> unit
(** To use at the end of a [.cmt]'s analysis: reset internal state *)
