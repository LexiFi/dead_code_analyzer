val collect_export_from_signature :
  path:Ident.t list ->
  comp_unit:string ->
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
  path:Ident.t list ->
  comp_unit:string ->
  Typedtree.structure
  -> unit
(** [collect_export_from_structure ~path ~comp_unit structure]
    recursively traverse the [structure] items and store exported elements
    (values, constructors, ...) in {!DeadCommon.decs} (or equivalent).
    See {!collect_export_from_signature} above for more information.
*)

val collect_export_from_include :
  path:Ident.t list ->
  Types.signature_item
  -> unit
(** [collect_export_from_include ~path sig_item]
    recursively traverse the [sig_item] and store exported elements
    (values, constructors, ...) in {!DeadCommon.incl}.
    See {!collect_export_from_signature} above for more information.
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
