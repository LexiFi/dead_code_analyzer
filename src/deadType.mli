(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2025 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

val decs : (Lexing.position, string * string) Hashtbl.t

val dependencies : (Lexing.position * Lexing.position) list ref

val to_string : Types.type_expr -> string
  (** [to_string typ] converts [typ] to its string representation in the toplevel *)

val check_style : Types.type_expr -> Lexing.position -> unit
  (** Look for bad style typing. (i.e. Argument expecting an optional argument) *)

val collect_eq_from_module_alias :
  rev_alias_path: string list ->
  original_path: Path.t ->
  sub_path: string list ->
  Types.type_declaration
  -> unit
(** [collect_eq_from_module_alias ~rev_alias_path ~original_path ~sub_path type_decl]
    stores equivalences between components of the [type_decl] defined in
    the current compilation unit at [sub_path] in the module defined at
    [List.rev rev_alias_path], and the same components in the same type
    at [sub_path] in the aliased module at [original_path].
    [rev_alias_path] must not be empty and is represented backward
    (i.e. the type name is at the head).
    If [is_internal = true] then there is an attempt to find the aliased
    definition in the current compilation unit (working upward from the
    alias path). If not found, then the behavior is the same as if
    [is_internal = false]: the aliased type is conidered external.

    E.g. the .mli declares [module M : sig type t = (* type_decl *) end]
         and the .ml [module M = N] and [N] also declares
         [type t = (* type_decl *)]
*)

val collect_eq_from_include :
  incl_path: Path.t ->
  path: string list ->
  Types.type_declaration
  -> unit
(** [collect_eq_from_include ~incl_path ~path type_decl]
    stores equivalences between components of the included [type_decl]
    defined in [incl_path] at [path] and the same components in the same
    type at [path_at_include @ path] in the current compilation unit if it is exported.
    [path] must not be empty and is represented forward (i.e. the type name
    is at the end).

    E.g. the .mli declares [type t = (* type_decl *)] and the .ml
         [include M] and [M] also declares [type t = (* type_decl *)]
*)

val tstr : Typedtree.type_declaration -> unit

val is_unit : Types.type_expr -> bool

val nb_args : keep:[> `All | `Lbl | `Opt | `Reg ] -> Types.type_expr -> int

val is_type : string -> bool

val collect_export :
  string list
  -> string
  -> (Lexing.position, string * string) Hashtbl.t
  -> Types.type_declaration
  -> unit

val correct_export : Types.type_declaration -> unit
(** Undo export collection for the specified type_decl *)

val collect_references :
  Lexing.position -> Lexing.position -> unit

val prepare_report: unit -> unit
(** To use at the end of the codebase analysis, before reporting.
    This merges all the references of equivalent types
*)

val report: unit -> unit
