(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2025 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)


open Typedtree



val last_class : Lexing.position ref


val collect_export :
  Ident.t list
  -> string
  -> (Lexing.position, string * string) Hashtbl.t
  -> ?obj: Types.type_expr
  -> ?cltyp: Types.class_type
  -> Location.t
  -> unit


val collect_references :
  meth: string
  -> call_site: Lexing.position
  -> expression
  -> unit


val add_equal :
  Lexing.position -> Lexing.position -> unit


val tstr :
  class_declaration * string list -> unit


val add_var :
  Lexing.position -> expression -> unit


val class_structure:
  class_structure -> unit


val class_field :
  class_field -> unit


val arg :
  Types.type_expr -> (Asttypes.arg_label * expression option) list -> unit


val coerce:
  expression -> Types.type_expr -> unit


val eom :
  unit -> unit


val report :
  unit -> unit
