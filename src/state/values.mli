(** Stateful manipulations related to exported values.
*)

type t

val create : unit -> t
(** Default value to start with *)

val add_exported_declaration :
  builddir:string ->
  val_loc:Lexing.position ->
  val_path:string ->
  t
  -> t
(** [add_exported_declaration ~builddir ~val_loc ~val_path vals]
    returns a [t] containing the same info as [vals], plus an extra
    declaration at [val_loc] in [builddir].
*)

val remove_exported_declaration :
  builddir:string ->
  val_loc:Lexing.position ->
  t
  -> t
(** [remove_exported_declaration ~builddir ~val_loc vals] returns a [t]
    containing the same info as [val] minus the declarations at [val_loc]
    in [builddir].
*)

val is_exported_declaration : val_loc:Lexing.position -> t -> bool
(** [is_exported_declaration ~val_loc vals] returns [true] if [val_loc] was
    added to [vals] via {!add_exported_declaration} above and not removed.
    Otherwise, it returns [false].
*)

val get_val_path :
  builddir:string ->
  val_loc:Lexing.position ->
  t
  -> string option
(** [get_val_path ~builddir ~val_loc vals] returns [Some val_path] if
    [val_loc] in [builddir] was added to [vals] via {!add_exported_declaration} above
    and not removed. The [val_path] is the one that was passed at the time.
    Otherwise, it returns [None].
*)

val add_use : val_loc:Lexing.position -> use_loc:Lexing.position -> t -> t
(** [add_use ~val_loc ~use_loc vals]
    returns a [t] containing the same the same info as [vals], plus an
    extra use of [val_loc] at [use_loc].
*)

val remove_uses : val_loc:Lexing.position -> t -> t
(** [remove_use ~val_loc vals]
    returns a [t] containing the same the same info as [vals], minus all
    the uses of [val_loc].
*)

val get_uses : val_loc:Lexing.position -> t -> Lexing.position list
(** [get_uses ~val_loc vals] returns all the [use_loc] that were associated
    with [val_loc] via {!add_use} above.
*)

val get_unused : ?max_uses:int -> t -> (int, (Lexing.position * string) list) Hashtbl.t
(** [get_unused ?max_uses vals] returns a table containing the [val_loc]
    in [builddir] added via {!add_exported_declaration} above and not
    removed, with at most [max_uses] [use_loc] associated via {!add_use}
    above.
    The key is the number of associated uses, and the value the list of
    [val_loc * builddir] used key number of times.
    By default, [max_uses = 0].
*)

val add_alias : orig_loc:Lexing.position -> alias_loc:Lexing.position -> t -> t
(** [add_alias ~orig_loc ~alias_loc vals] returns a [t] containing the
    same info as [vals], plus an extra equivalence between [orig_loc] and
    [alias_loc].
    This equivalence implies that a use of either is a use of both. In
    particular, a use of the [alias_loc] is a use of the [orig_loc].
*)
