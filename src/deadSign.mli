type context =
  | Toplevel
  | In_module of (Ident.t * Location.t)
  | In_modtyp of (Ident.t * Location.t)
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
