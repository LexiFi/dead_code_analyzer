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
    of [meth_name] in [builddir] added via {!add_exported_declaration} above
    and not removed, with at most [max_uses] [use_loc] associated via {!add_use} above.
    The key is the number of associated uses, and the value the list of
    [obj_loc * meth_name * builddir] used key number of times.
    By default, [max_uses = 0].
*)

val add_alias : orig_loc:Lexing.position -> alias_loc:Lexing.position -> ?meth_name:string -> t -> t
(** [add_alias ~orig_loc ~alias_loc ?meth_name meths] returns a [t] containing the
    same info as [meths], plus an extra equivalence between [orig_loc] and
    [alias_loc].
    This equivalence implies that a use of either is a use of both. In
    particular, a use of the [alias_loc] is a use of the [orig_loc].
    If a [meth_name] is provided, then the equivalence only applies to it.
*)
