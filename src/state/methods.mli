(** Stateful manipulations related to exported methods.
*)

type t

val create : unit -> t
(** Default method to start with *)

val add_exported_declaration :
  builddir:string ->
  obj_loc:Lexing.position ->
  meth_name:string ->
  meth_path:string ->
  t
  -> t
(** [add_exported_declaration ~builddir ~obj_loc ~meth_name ~meth_path meths]
    returns a [t] containing the same info as [meths], plus an extra
    declaration at [obj_loc] in [builddir] for method [meth_name].
    NOTE: [obj_loc] is the location of the object/class owning [meth_name].
*)

val remove_exported_declaration :
  ?builddir:string ->
  obj_loc:Lexing.position ->
  meth_name: string ->
  t
  -> t
(** [remove_exported_declaration ?builddir ~obj_loc ~meth_name meths]
    returns a [t] containing the same info as [meths] minus the
    declaration of [meth_name] at [obj_loc] in [builddir].
    If [builddir = None], then the [meth_name] at [obj_loc] in all the
    builddirs are removed.
*)

val remove_exported_declarations :
  ?builddir:string ->
  obj_loc:Lexing.position ->
  t
  -> t
(** [remove_exported_declarations ?builddir ~obj_loc meths]
    is the same as calling {!remove_exported_declaration} above for each
    [meth_name] of the object.
*)

val is_exported_declaration : obj_loc:Lexing.position -> t -> bool
(** [is_exported_declaration ~obj_loc meths] returns [true] if [obj_loc] was
    added to [meths] via {!add_exported_declaration} above and not removed.
    Otherwise, it returns [false].
*)

val get_meth_path :
  builddir:string ->
  obj_loc:Lexing.position ->
  meth_name:string ->
  t
  -> string option
(** [get_meth_path ~builddir ~obj_loc ~meth_name meths] returns
    [Some meth_path] if [obj_loc] in [builddir] was added to [meths] via
    {!add_exported_declaration} above and not removed. The [meth_path] is
    the one passed at the time for [meth_name].
    Otherwise, it returns [None].
*)

val add_use :
  obj_loc:Lexing.position ->
  meth_name:string ->
  use_loc:Lexing.position ->
  t
  -> t
(** [add_use ~obj_loc ~meth_name ~use_loc meths]
    returns a [t] containing the same the same info as [meths], plus an
    extra use at [use_loc] of [meth_name] declared at [obj_loc].
*)

val remove_uses : obj_loc:Lexing.position -> ?meth_name:string -> t -> t
(** [remove_use ~obj_loc ?meth_name meths]
    returns a [t] containing the same the same info as [meths], minus all
    the uses of [obj_loc].
    If a [meth_name] is provided, then only uses associated to this name
    are removed.
*)

val get_uses :
  obj_loc:Lexing.position ->
  meth_name:string ->
  t
  -> Lexing.position list
(** [get_uses ~obj_loc ~meth_name meths] returns all the [use_loc] that
    were associated with [obj_loc] and [meth_name] via {!add_use} above.
*)

val get_unused :
  ?max_uses:int ->
  t
  -> (int, (Lexing.position * string * string) list) Hashtbl.t
(** [get_unused ?max_uses meths] returns a table containing the [obj_loc]
    of [meth_name] in [builddir] added via {!add_exported_declaration}
    above, marked defined via {!mark_defined} below, and not removed, with
    at most [max_uses] [use_loc] associated via {!add_use} above.
    The key is the number of associated uses, and the value the list of
    [obj_loc * meth_name * builddir] used key number of times.
    By default, [max_uses = 0].
*)

val add_alias : orig_loc:Lexing.position -> alias_loc:Lexing.position -> t -> t
(** [add_alias ~orig_loc ~alias_loc meths] returns a [t] containing the
    same info as [meths], plus an extra alias at [alias_loc] of [orig_loc].
*)

val resolve_aliases : t -> t
(** [resolve_aliases meths] returns a [t] containing the same info as [meths]
    with all the uses of aliases copied as uses of the original declaration,
    and without [alias_loc] among the declarations.
    This function is meant to be called once before calling {!get_unused}.
*)

val mark_defined :
  builddir:string ->
  obj_loc:Lexing.position ->
  meth_name:string ->
  t
  -> t
(** [mark_defined ~builddir ~obj_loc ~meth_name meths]
    returns a [t] containing the same info as [meths], plus an indication
    that the definition for [meth_name] at [obj_loc] in [builddir] was
    encountered.
    This overrides any previous status for that method.
*)

val mark_inherited :
  builddir:string ->
  obj_loc:Lexing.position ->
  meth_name:string ->
  inherited_path:string ->
  t
  -> t
(** [mark_inherited ~builddir ~obj_loc ~meth_name ~inherited_path meths]
    returns a [t] containing the same info as [meths], plus an extra
    inheritance status : [meth_name] at [obj_loc] in [builddir] is actually
    inherited from [inherited_path].
    This overrides any previous status for that method.
*)

val mark_virtual :
  builddir:string ->
  obj_loc:Lexing.position ->
  meth_name:string ->
  t
  -> t
(** [mark_virtual ~builddir ~obj_loc ~meth_name meths]
    returns a [t] containing the same info as [meths], plus an indication
    that [meth_name] at [obj_loc] in [builddir] is virtual encountered.
    This overrides any previous status for that method.
*)

val add_loc_binding : obj_path:string -> obj_loc:Lexing.position -> t -> t
(** [add_loc_binding ~obj_path ~obj_loc meths] returns a [t] containing
    the same info as [meths], plus an extra binding of [obj_path] to [obj_loc].
    All the previous bindings of [obj_path] are shadowed.

    Retrieving the latest binding is done via {!find_loc} below.
    Those 2 functions are intended to resolve class locations when they
    are not readily available (e.g. in [inherit] fields).
*)

val find_loc : obj_path:string -> t -> Lexing.position option
(** [find_loc ~obj_path meths] returns [Some obj_loc] if the [obj_loc] was
    added via {!add_loc_binding} above.
    Otherwise, it returns [None].
*)

val get_orig_loc : obj_loc:Lexing.position -> t -> Lexing.position
(** [get_orig_loc ~obj_loc meths] returns the location of the original
    declaration of the object/class defined at [obj_loc].
    It does this by following the aliases added via {!add_alias} above.
*)
