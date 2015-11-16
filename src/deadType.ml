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


let rec _TO_STRING_ typ = match typ.desc with
  | Tvar i -> begin match i with Some id -> id | None -> "'a" end
  | Tarrow (_, t1, t2, _) ->
      begin match t1.desc with
        | Tarrow _ -> "(" ^ _TO_STRING_ t1 ^ ")"
        | _ -> _TO_STRING_ t1 end
      ^ " -> " ^ _TO_STRING_ t2
  | Ttuple l -> begin match l with
      | e::l ->
          List.fold_left (fun prev typ -> prev ^ " * " ^ _TO_STRING_ typ) (_TO_STRING_ e) l
      | [] -> "*" end
  | Tconstr (path, l, _) -> make_name path l
  | Tobject (self, _) -> "< " ^ _TO_STRING_ self ^ " >"
  | Tfield (s, k, _, t1) ->
      if Btype.field_kind_repr k = Fpresent then
        s
        ^ begin match t1.desc with
            | Tfield _ -> "; " ^ _TO_STRING_ t1
            | _ -> "" end
      else _TO_STRING_ t1
  | Tnil -> "Tnil"
  | Tlink t -> _TO_STRING_ t
  | Tsubst _ -> "Tsubst _"
  | Tvariant {row_more; _} -> _TO_STRING_ row_more
  | Tunivar _ -> "Tunivar _"
  | Tpoly (t, _) -> _TO_STRING_ t
  | Tpackage _ -> "Tpackage _"


and make_name path l =
  let t = match l with
    | [] -> ""
    | _ -> List.fold_left (fun prev typ -> prev ^ _TO_STRING_ typ ^ " ") "" l;
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


let is_type s =
  let rec blk s p l acc =
    try
      if s.[p] = '.' then
        let acc = String.sub s (p - l) l :: acc in
        blk s (p + 1) 0 acc
      else blk s (p + 1) (l + 1) acc
    with _ -> String.sub s (p - l) l :: acc
  in
  if not (String.contains s '.') then false
  else
    let [@ocaml.warning "-8"] hd::cont::_ = blk s 0 0 [] in
    String.capitalize_ascii hd = hd || String.lowercase_ascii cont = cont



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


let report () =
  let rec report nb_call =
    let l =
      let folder = fun acc (fn, path, loc) ->
        let rec cut_main s pos =
          if pos = String.length s then s
          else if s.[pos] = '.' then String.sub s (pos + 1) (String.length s - pos - 1)
          else cut_main s (pos + 1)
        in
        let test elt = match Hashtbl.find references elt with
          | l when check_length nb_call l -> Some ((fn, cut_main path 0, loc, l) :: acc)
          | _ -> None
        in
        let test () =
          if String.contains path '#' || not (is_type path) then None
          else
            match test loc with
              | None -> None
              | opt ->
                let locs = Hashtbl.find corres loc in
                match List.find (fun node -> try test node <> None with Not_found -> false) locs with
                  | exception Not_found -> opt
                  | loc -> test loc
        in match test () with
          | exception Not_found when nb_call = 0 ->
                (fn, cut_main path 0, loc, []) :: acc
          | exception Not_found -> acc
          | None -> acc
          | Some l -> l
      in
      List.fold_left folder [] !decs
      |> List.fast_sort (fun (fn1, path1, loc1, _) (fn2, path2, loc2, _) ->
          compare (fn1, loc1, path1) (fn2, loc2, path2))
    in

    let change =
      let (fn, _, _, _) = try List.hd l with _ -> ("_none_", "", !last_loc, []) in
      dir fn
    in
    let pretty_print = fun (fn, path, loc, call_sites) ->
      if change fn then print_newline ();
      prloc ~fn loc;
      print_string path;
      if call_sites <> [] && !DeadFlag.typ.call_sites then print_string "    Call sites:";
      print_newline ();
      if !DeadFlag.typ.call_sites then begin
        List.iter (pretty_print_call ()) call_sites;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call = nb_call < !DeadFlag.typ.threshold in
    let s =
      if nb_call = 0 then "UNUSED RECORD FIELDS/VARIANT CONSTRUCTORS"
      else "ALMOST UNUSED RECORD FIELDS/VARIANT CONSTRUCTORS"
    in
    DeadCommon.report s l continue nb_call pretty_print report

  in report 0
