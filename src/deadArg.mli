(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)


open Typedtree



(* Functions needing a location in the current file to be processed
 * before being executed.
 * It is known that this location will have been processed at the end of
 * the binding.
 * Needed because the Tast_mapper run through sequences from the end
 * because tuples are built from right to left*)
val later : (unit -> unit) list ref

(* Functions needing a location out of the current file to be processed
 * before being executed. *)
val last : (unit -> unit) list ref


(* Self cleaning *)
val eom :
  unit -> unit


(* Add all optional arguments met if they are used to match a signature or the location
 * is not a ghost and they are part of the application (w/ or w/o value) *)
val process :
  Lexing.position
  -> (Asttypes.arg_label * expression option) list
  -> unit


(* Constructs the opt_args field of the given node *)
val node_build :
  Lexing.position -> Typedtree.expression -> unit
