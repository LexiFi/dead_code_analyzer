(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

val dependencies : (Location.t * Location.t) list ref

val to_string : Types.type_expr -> string
  (** [to_string typ] converts [typ] to its string representation in the toplevel *)

val match_str : Types.type_expr -> string -> bool
  (** [match_str typ str] checks if a [typ] matches [str].
    [str] must be formated as the toplevel representation representation of the expected type. *)

val check_style : Types.type_expr -> Location.t -> unit
  (** Look for bad style typing. (i.e. Argument expecting an optional argument) *)

val tstr : Typedtree.type_declaration -> unit

val is_unit : Types.type_expr -> bool

val collect_export :
  Ident.t list
  -> string
  -> Types.type_declaration
  -> unit
