type context =
  | Toplevel
  | In_module of (Ident.t * Location.t)
  | Include

val collect_export :
  context:context ->
  path:Ident.t list ->
  comp_unit:string ->
  stock:(Lexing.position, string * string) Hashtbl.t ->
  Types.signature_item
  -> unit
(** [collect_export ~context ~path ~comp_unit ~stock sig_item]
    recursively traverse the [sig_item] and store exported elements
    (values, constructors, ...) in [stock].
    - [path] is the current module path. E.g. for a value in a submodule,
      the path is [Module.Submodule].
    - [comp_unit] is the compilation unit of the [sig_item].
    - [context] specifies if the current signature item is found at the
      [Toplevel] of the current compilation unit, in a module, a modtype,
      or an [Include]
*)

val correct_export : Types.signature_item -> unit
(** Unexport the signature_item. This is used to correct wrongful exports from
    {!collect_export} above
*)

val collect_export_from_typedtree :
  path:Ident.t list ->
  comp_unit:string ->
  Typedtree.signature
  -> unit
(** [collect_export_from_typedtree ~path ~comp_unit sigature]
    recursively traverse the [signature] items and store exported elements
    (values, constructors, ...) in {!DeadCommon.desc}.
    See {!collect_export} above for more information.
*)

val modtype :
  on_mismatch: (Types.signature -> unit) ->
  Typedtree.module_type
  -> unit
(** [modtype ~on_mismatch mt] checks that the Typedtree and Types signatures
    for [mt] agree. I.e. if the first one is explicit when the second one is.
    If not, then the first one is implicit and [on_mismatch] is called on the
    second. *)

val collect_export_from_structure :
  path:Ident.t list ->
  comp_unit:string ->
  Typedtree.structure
  -> unit
