(** Stateful manipulations related to exported constructors and fields.
*)

type t

val create : unit -> t
(** Default value to start with *)

val add_exported_declaration :
  builddir:string ->
  cf_loc:Lexing.position ->
  cf_path:string ->
  t
  -> t
(** [add_exported_declaration ~builddir ~cf_loc ~cf_path ctors_fields]
    returns a [t] containing the same info as [ctors_fields], plus an extra
    declaration at [cf_loc] in [builddir].
*)

val remove_exported_declaration :
  builddir:string ->
  cf_loc:Lexing.position ->
  t
  -> t
(** [remove_exported_declaration ~builddir ~cf_loc ctors_fields] returns a [t]
    containing the same info as [val] minus the declarations at [cf_loc]
    in [builddir].
*)

val is_exported_declaration : cf_loc:Lexing.position -> t -> bool
(** [is_exported_declaration ~cf_loc ctors_fields] returns [true] if [cf_loc]
    was added to [ctors_fields] via {!add_exported_declaration} above and not
    removed.
    Otherwise, it returns [false].
*)

val get_cf_path :
  builddir:string ->
  cf_loc:Lexing.position ->
  t
  -> string option
(** [get_cf_path ~builddir ~cf_loc ctors_fields] returns [Some cf_path] if
    [cf_loc] in [builddir] was added to [ctors_fields] via
    {!add_exported_declaration} above and not removed. The [cf_path] is
    the one that was passed at the time.
    Otherwise, it returns [None].
*)

val add_use : cf_loc:Lexing.position -> use_loc:Lexing.position -> t -> t
(** [add_use ~cf_loc ~use_loc ctors_fields]
    returns a [t] containing the same the same info as [ctors_fields], plus an
    extra use of [cf_loc] at [use_loc].
*)

val remove_uses : cf_loc:Lexing.position -> t -> t
(** [remove_use ~cf_loc ctors_fields]
    returns a [t] containing the same the same info as [ctors_fields], minus all
    the uses of [cf_loc].
*)

val get_uses : cf_loc:Lexing.position -> t -> Lexing.position list
(** [get_uses ~cf_loc ctors_fields] returns all the [use_loc] that were
    associated with [cf_loc] via {!add_use} above.
*)

val get_unused :
  ?max_uses:int ->
  t
  -> (int, (Lexing.position * string) list) Hashtbl.t
(** [get_unused ?max_uses ctors_fields] returns a table containing the [cf_loc]
    in [builddir] added via {!add_exported_declaration} above and not
    removed, with at most [max_uses] [use_loc] associated via {!add_use}
    above.
    The key is the number of associated uses, and the value the list of
    [cf_loc * builddir] used key number of times.
    By default, [max_uses = 0].
*)

val add_alias : orig_loc:Lexing.position -> alias_loc:Lexing.position -> t -> t
(** [add_alias ~orig_loc ~alias_loc ctors_fields] returns a [t] containing the
    same info as [ctors_fields], plus an extra equivalence between [orig_loc]
    and [alias_loc].
    This equivalence implies that a use of either is a use of both. In
    particular, a use of the [alias_loc] is a use of the [orig_loc].
*)
