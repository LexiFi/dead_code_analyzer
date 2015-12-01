(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Extensions internally used at Lexifi. *)


(*   .^.  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  .^.   *)
(*  / ! \  DO NOT DELETE UNLESS YOU CAN COMPILE WITH `make lexifi' AND YOU KNOW WHAT YOU ARE DOING  / ! \  *)
(* /_____\   /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\   /_____\ *)


open Parsetree
open Types
open Typedtree

open DeadCommon


                (********   ATTRIBUTES   ********)

let ftyp = Hashtbl.create 256   (* Link from field to type *)
let dyn_rec = ref []            (* Record names used for dynamic typing and locations of those uses *)
let str = Hashtbl.create 256
let used = ref []


let export_type path ld_id typ =
  Hashtbl.add
    ftyp
    (String.concat "." (List.rev_map (fun id -> id.Ident.name) (ld_id::path)))
    typ


let value_binding vb = match vb.vb_pat.pat_desc with
  | Tpat_var _ -> begin match vb.vb_expr.exp_desc with
    | Texp_constant (Asttypes.Const_string (s, _)) ->
        hashtbl_add_to_list str s vb.vb_pat.pat_loc
    | _ -> () end
  | _ -> ()

let sig_value (value : Types.value_description) =

  let add strct = match strct.pstr_desc with
    | Pstr_eval ({pexp_desc=Pexp_constant (Const_string (s, _)); _}, _) -> hashtbl_add_to_list str s value.val_loc
    | _ -> ()
  in

  let add = function
    | ({Asttypes.txt="mlfi.val_expr"; _}, PStr structure) ->
        List.iter add structure
    | _ -> ()
  in

  List.iter add value.val_attributes


let type_ext ct = match ct.ctyp_desc with
    | Ttyp_props (props, _) ->
        List.iter
          (fun (_, strin) ->
            used := (strin, ct.ctyp_loc) :: !used;
          )
          props
    | _ -> ()


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
  List.iter
    (fun (strin, pos) ->
      hashtbl_find_list str strin
      |> List.iter (fun loc -> hashtbl_add_to_list references loc pos)
    )
    !used;
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
            hashtbl_add_to_list references loc call_site
          )
          (proc name);
    | _ -> ()
  in
  List.iter process !dyn_rec




let () =
  DeadLexiFi.value_binding := value_binding;
  DeadLexiFi.sig_value := sig_value;
  DeadLexiFi.export_type := export_type;
  DeadLexiFi.type_ext := type_ext;
  DeadLexiFi.tstr_type := tstr_type;
  DeadLexiFi.ttype_of := ttype_of;
  DeadLexiFi.prepare_report := prepare_report
