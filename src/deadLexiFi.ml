(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Extensions internally used at Lexifi. *)

open Types
open Typedtree

open DeadCommon


                (********   ATTRIBUTES   ********)

let ftyp = Hashtbl.create 256   (* Link from field to type *)
let dyn_rec = ref []            (* Record names used for dynamic typing and locations of those uses *)


let export_type path ld_id typ =
  Hashtbl.add
    ftyp
    (String.concat "." (List.rev_map (fun id -> id.Ident.name) (ld_id::path)))
    typ


let tstr_type typ ld_name ctype =
  let path = String.concat "." @@ List.rev @@
    ld_name.Asttypes.txt
    :: typ.typ_name.Asttypes.txt :: !mods
    @ (String.capitalize_ascii (unit !current_src):: [])
  in
  if not (Hashtbl.mem fields path) then
    Hashtbl.add ftyp path ctype


let ttype_of e =
  let name = String.concat "." @@ List.rev @@
    !mods @ (String.capitalize_ascii (unit !current_src):: [])
  in
  dyn_rec := (name, e.exp_type, (if e.exp_loc.Location.loc_ghost then !last_loc else e.exp_loc)) :: !dyn_rec


let prepare_report () =
  let rec process (p, typ, call_site) = match typ.desc with
    | Tarrow (_, t, _, _) | Tlink t -> process (p, t, call_site)
    | Tconstr (path, _, _) ->
        let name = Path.name path in
        let name =
          if String.contains name '.' then name
          else p ^ "." ^ name
        in
        let met = ref [] in
          let rec proc name =
          if not (List.mem name !met) then begin
            let len = String.length name in
            met := name :: !met;
            Hashtbl.fold
              (fun key loc acc ->
                if String.length key > len && String.sub key 0 len = name && key.[len] = '.' then
                  loc :: (try proc (Hashtbl.find ftyp key) with _ -> []) @ acc
                else acc)
              fields
              []
          end
          else []
        in
        List.iter
          (fun loc ->
            Hashtbl.replace
              references
              loc
              (call_site :: hashtbl_find_list references loc))
          (proc name);
    | _ -> ()
  in
  List.iter process !dyn_rec
