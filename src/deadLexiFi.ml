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

let dyn_rec = ref []            (* Record names used for dynamic typing and locations of those uses *)
let str = Hashtbl.create 256
let used = ref []

let field_link = Hashtbl.create 256
let dyn_used = Hashtbl.create 256

let is_user_defined s =
  let l = [_variant; "bool"; "float"; "int"; "string"; "unit"] in
  not (String.contains s ' ')
  && (s <> String.capitalize_ascii s && not (List.mem s l)
    || String.contains s '.' && string_cut '.' s <> "Pervasives")


let sig_value (value : Types.value_description) =

  let add strct = match strct.pstr_desc with
    | Pstr_eval ({pexp_desc=Pexp_constant (Const_string (s, _)); _}, _) ->
        hashtbl_add_unique_to_list str s value.val_loc
    | _ -> ()
  in

  let add = function
    | ({Asttypes.txt="mlfi.value_approx"; _}, PStr structure) ->
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


let tstr_type typ ctype =
  let path = String.concat "." @@ List.rev @@
    typ.typ_name.Asttypes.txt :: !mods
    @ (String.capitalize_ascii (unit !current_src):: [])
  in
  if is_user_defined ctype then begin
    hashtbl_add_to_list field_link path ctype;
  end


let ttype_of e =
  let name = String.concat "." @@ List.rev @@
    !mods @ (String.capitalize_ascii (unit !current_src):: [])
  in
  dyn_rec := (name, e.exp_type, (if e.exp_loc.Location.loc_ghost then !last_loc else e.exp_loc)) :: !dyn_rec


let prepare_report decs =
  List.iter
    (fun (strin, pos) ->
      hashtbl_find_list str strin
      |> List.iter (fun loc -> if exported DeadFlag.typ loc then hashtbl_add_to_list references loc pos)
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
            met := name :: !met;
            name :: List.fold_left (fun acc name -> acc @ (proc name)) [] (hashtbl_find_list field_link name)
          end
          else []
        in
        List.iter
          (fun typ ->
            hashtbl_add_to_list dyn_used typ call_site
          )
          (proc name);
    | _ -> ()
  in
  List.iter process !dyn_rec;
  Hashtbl.iter
    (fun loc (_, path) ->
      let rec get_type s pos =
        if pos = 0 then s
        else if s.[pos] = '.' then String.sub s 0 pos
        else get_type s (pos - 1)
      in
      List.iter
        (if exported DeadFlag.typ loc then hashtbl_add_to_list references loc else ignore)
        (hashtbl_find_list dyn_used (get_type path (String.length path - 1)))
    )
    decs




let () =
  DeadLexiFi.sig_value := sig_value;
  DeadLexiFi.type_ext := type_ext;
  DeadLexiFi.tstr_type := tstr_type;
  DeadLexiFi.ttype_of := ttype_of;
  DeadLexiFi.prepare_report := prepare_report
