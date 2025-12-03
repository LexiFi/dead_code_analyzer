(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2025 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

open Typedtree

val later : (unit -> unit) list ref
(** Functions needing a location in the current file to be processed
    before being executed.
    It is known that this location will have been processed at the end of
    the binding.
    Needed because the Tast_mapper run through sequences from the end
    because tuples are built from right to left*)

val last : (unit -> unit) list ref
(** Functions needing a location out of the current file to be processed
    before being executed. *)

val eom : unit -> unit
(** Self cleaning *)

val register_uses :
  Lexing.position -> (Asttypes.arg_label * expression option) list -> unit
(** An optional argument is used if it is required match a signature, or if it
    is part of an application (w/ or w/o value) *)

val bind : Lexing.position -> Typedtree.expression -> unit
(** Bind the opt parameters of expr to the given position *)
