(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)


open Typedtree



val later : (unit -> unit) list ref


val wrap :
  ('a -> 'b -> unit)
  -> 'a
  -> 'b
  -> unit


val process :
  Location.t
  -> (Asttypes.arg_label * expression option * optional) list
  -> unit

val node_build :
  DeadCommon.vd_node -> Typedtree.expression -> unit
