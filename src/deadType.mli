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

val tstr : Typedtree.type_declaration -> unit

val is_unit : Types.type_expr -> bool

val nb_args : keep:[> `All | `Lbl | `Opt | `Reg ] -> Types.type_expr -> int

val is_type : string -> bool

val collect_export :
  Ident.t list
  -> string
  -> (Lexing.position, string * string) Hashtbl.t
  -> Types.type_declaration
  -> unit

val collect_references :
  Lexing.position -> Lexing.position -> unit

val report: unit -> unit
