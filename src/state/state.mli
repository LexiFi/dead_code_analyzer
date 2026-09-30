(** Stateful info of the analysis *)

module File_infos = File_infos

module Values = Values
module Methods = Methods
module Ctors_fields = Ctors_fields

type t =
  { config : Config.t (** Configuration of the analysis *)
  ; comp_unit_to_path : (string, string) Hashtbl.t
      (** Compilation unit -> filepaths. Useful for quick filepath retrieval *)
  ; file_infos : File_infos.t (** Info about the file being analyzed *)
  ; values : Values.t (** Info about exported values declarations and uses *)
  ; methods : Methods.t (** Info about exported methods declarations and uses *)
  ; ctors_fields : Ctors_fields.t
      (** Info about exported constructors and fields *)
  }

val init : Config.t -> t
(** [init config] initial state for an analysis configured by [config] *)

val update_config : Config.t -> t -> t
(** [update_config config state] changes the analysis configuration *)

val change_file : t -> string -> (t, string) result
(** [change_file state cmti_file] prepare the analysis to move on to [cmti_file].
    See [File_infos.change_file] for error cases. *)

val add_exported_declaration:
  elt_kind:[< `Ctor_field | `Method of string | `Value ] ->
  elt_loc:Lexing.position ->
  elt_path:string ->
  t
  -> t
(** [add_exported_declaration ~elt_kind ~elt_loc ~elt_path state] stores
    a new exported element at [elt_loc] in the current builddir with fully
    qualified path [elt_path].
    The element may be discarded if the [elt_kind]'s corresponding report section
    is disabled.
    If [elt_kind] is a [Method], then it must be payloaded with the method's
    name, the [elt_loc] is the one of the owning object/class, and the
    [elt_path] the method's path.

    This function is preferred over directly the corresponding element kind's
    dedicated function (e.g. [Values.add_exported_declaration]).
*)

val is_exported_declaration:
  elt_kind:[< `Ctor_field | `Object | `Value ] ->
  elt_loc:Lexing.position ->
  t
  -> bool
(** [is_exported_declaration ~elt_kind ~elt_loc state] returns [true]
    if the provided [elt_loc] is stored as an exported declaration for
    the given [elt_kind]
    If [elt_kind] is an [Object], then it returns true if any of its methods
    was stored as an exported declaration (see {!add_exported_declaration}
    above)

    This function is preferred over directly the corresponding element kind's
    dedicated function (e.g. [Values.is_exported_declaration]).
*)

val add_use:
  elt_kind:[< `Ctor_field | `Method of string | `Value ] ->
  elt_loc:Lexing.position ->
  use_loc:Lexing.position ->
  t
  -> t
(** [add_use ~elt_kind ~elt_loc ~use_loc state] stores a use of [elt_loc]
    at [use_loc].
    The use may be discarded if the [elt_kind]'s corresponding report section
    is disabled or if the use is out of scope (e.g. an internal value use
    when --internal is not configured).
    If [elt_kind] is a [Method], then it must be payloaded with the method's
    name and the [elt_loc] is the one of the owning object/class.

    This function is preferred over directly the corresponding element kind's
    dedicated function (e.g. [Values.add_use]).
*)

val add_self_use:
  elt_kind:[ `Method of string ] ->
  elt_loc:Lexing.position ->
  use_loc:Lexing.position ->
  t
  -> t
(** [add_self_use ~elt_kind ~elt_loc ~use_loc] is similar to {!add_use] above
    but for self-referencing uses within an object or class definition.
    Only [Method] is accepted as [elt_kind].
*)

val add_alias:
  elt_kind:[< `Ctor_field | `Object | `Value ] ->
  orig_loc:Lexing.position ->
  alias_loc:Lexing.position ->
  t
  -> t
(** [add_alias ~elt_kind ~orig_loc ~alias_loc state] stores an alias at
    [alias_loc] for [orig_loc].
    This implies that any use of [alias_loc] is equivalent to a use of [obj_loc]
    The use may be discarded if the [elt_kind]'s corresponding report section
    is disabled.

    This function is preferred over directly the corresponding element kind's
    dedicated function (e.g. [Values.add_alias]).
*)

val get_current : unit -> t
(** [get_current ()] returns the state used during the analysis. *)

val update : t -> unit
(** [update state] replaces the analysis' state with [state]. *)
