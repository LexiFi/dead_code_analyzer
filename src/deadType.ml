(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

open Types
open Typedtree

open DeadCommon



                (********   ATTRIBUTES  ********)

let dependencies = ref []   (* like the cmt value_dependencies but for types *)

let fields : (string, Location.t) Hashtbl.t = Hashtbl.create 256      (* link from fields (record/variant) paths and locations *)



                (********   HELPERS   ********)

let is_unit t = match (Ctype.repr t).desc with
  | Tconstr (p, [], _) -> Path.same p Predef.path_unit
  | _ -> false


let rec to_string typ = match typ.desc with
  | Tvar i -> begin match i with Some id -> id | None -> "'a" end
  | Tarrow (_, t1, t2, _) ->
      begin match t1.desc with
        | Tarrow _ -> "(" ^ to_string t1 ^ ")"
        | _ -> to_string t1 end
      ^ " -> " ^ to_string t2
  | Ttuple l -> begin match l with
      | e::l ->
          List.fold_left (fun prev typ -> prev ^ " * " ^ to_string typ) (to_string e) l
      | [] -> "*" end
  | Tconstr (path, l, _) -> make_name path l
  | Tobject (self, _) -> "< " ^ to_string self ^ " >"
  | Tfield (s, k, t, t1) ->
      if Btype.field_kind_repr k = Fpresent then
        s ^ ": "
        ^ to_string t
        ^ begin match t1.desc with
            | Tfield _ -> "; " ^ to_string t1
            | _ -> "" end
      else to_string t1
  | Tnil -> "Tnil"
  | Tlink t -> to_string t
  | Tsubst _ -> "Tsubst _"
  | Tvariant {row_more; _} -> to_string row_more
  | Tunivar _ -> "Tunivar _"
  | Tpoly (t, _) -> to_string t
  | Tpackage _ -> "Tpackage _"


and make_name path l =
  let t = match l with
    | [] -> ""
    | _ -> List.fold_left (fun prev typ -> prev ^ to_string typ ^ " ") "" l;
  in
  let name = Path.name path in
  let len =
    let open Path in
    let rec len path = match path with
      | Pident id when id.Ident.name = "Pervasives" -> String.length "Pervasives."
      | Papply (_, _) | Pident _ -> 0
      | Pdot (path, _, _) -> len path
    in len path
  in
  t ^ String.sub name len (String.length name - len)


let match_str typ str =
  let typ = to_string typ in
  let str =
    let rec single_space s pos =
      if pos = String.length s - 1 then String.make 1 s.[pos]
      else if s.[pos] = ' ' && s.[pos + 1] = ' ' then single_space s (pos + 1)
      else String.make 1 s.[pos] ^ single_space s (pos + 1)
    in
    single_space str 0
  in

  let rec get_block str pos len =
    if pos = String.length str || str.[pos] = ' ' then
      String.sub str (pos - len) len
    else get_block str (pos + 1) (len + 1)
  in

  let rec compare typ str pos1 pos2 =
    try
      let t = get_block typ pos1 0 in
      let s = get_block str pos2 0 in
      if s <> "" && s <> t then false
      else compare typ str (pos1 + String.length t + 1) (pos2 + String.length s + 1)
    with _ -> (pos1 >= String.length typ) = (pos2 >= String.length str)
  in compare typ str 0 0



                (********   PROCESSING  ********)

let collect_export path u t =

  let save id loc =
    if t.type_manifest = None then
      export path u id loc;
    let path = String.concat "." @@ List.rev_map (fun id -> id.Ident.name) (id::path) in
    if Hashtbl.mem fields path then
      Hashtbl.replace corres loc
        (let loc = Hashtbl.find fields path in
        loc :: hashtbl_find_list corres loc);
    Hashtbl.replace fields path loc
  in

  match t.type_kind with
    | Type_record (l, _) ->
        List.iter (fun {Types.ld_id; ld_loc; _} -> save ld_id ld_loc) l
    | Type_variant l ->
        List.iter (fun {Types.cd_id; cd_loc; _} -> save cd_id cd_loc) l
    | _ -> ()


(* Look for bad style typing *)
let rec check_style t loc = if !DeadFlag.style.opt_arg then match t.desc with
  | Tlink t -> check_style t loc
  | Tarrow (lab, _, t, _) -> begin match lab with
    | Optional lab when check_underscore lab ->
        style := (!current_src, loc, "val f: ... -> (... -> ?_:_ -> ...) -> ...") :: !style
    | _ -> check_style t loc end
  | _ -> ()


let tstr typ =

  let assoc name loc =
    let path = String.concat "." @@ List.rev @@
      name.Asttypes.txt
      :: typ.typ_name.Asttypes.txt :: !mods
      @ (String.capitalize_ascii (unit !current_src):: [])
    in
    begin try match typ.typ_manifest with
      | Some {ctyp_desc=Ttyp_constr (_, {txt;  _}, _); _} ->
          let loc1 = Hashtbl.find fields
            (String.concat "." @@
              String.capitalize_ascii (unit !current_src)
              :: Longident.flatten txt
              @ (name.Asttypes.txt :: []))
          in
          let loc2 = Hashtbl.find fields path in
          dependencies :=
          (loc2, loc1) :: (loc1, loc) :: !dependencies;
      | _ -> ()
    with _ -> () end;
    try
      let loc1 = Hashtbl.find fields path in
      dependencies := (loc1, loc) :: !dependencies
    with Not_found -> Hashtbl.add fields path loc
  in

  match typ.typ_kind with
    | Ttype_record l ->
        List.iter (fun {Typedtree.ld_name; ld_loc; _} -> assoc ld_name ld_loc) l
    | Ttype_variant l ->
        List.iter (fun {Typedtree.cd_name; cd_loc; _} -> assoc cd_name cd_loc) l
    | _ -> ()
