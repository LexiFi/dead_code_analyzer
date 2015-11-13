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

let references = Hashtbl.create 256                     (* references by path#name *)

let self_ref = Hashtbl.create 256                       (* references by path#name to itself *)

let super_ref = Hashtbl.create 256                      (* references by path#name to super *)

let content = Hashtbl.create 256                        (* classname -> [field names] *)

let class_dependencies = Hashtbl.create 256             (* inheritance links *)

let defined = ref []                                    (* all classes defined in the current file *)

let aliases = ref []                                    (* aliases on class names *)

let last_class = ref ""                                 (* last class met *)

let last_field = ref ""                                 (* last field met *)



                (********   HELPERS   ********)


let rec sign = function
  | Cty_signature sg -> sg
  | Cty_arrow (_, _, t)
  | Cty_constr (_, _, t) -> sign t


let rec treat_fields action typ = match typ.desc with
  | Tobject (t, _)
  | Tlink t -> treat_fields action t
  | Tfield (s, k, _, t) ->
      if Btype.field_kind_repr k = Fpresent then
        action s;
      treat_fields action t
  | _ -> ()


let rec cut_sharp s pos len =
  if len = String.length s then s
  else if s.[pos] = '#' then String.sub s (pos - len) len
  else cut_sharp s (pos + 1) (len + 1)


let rec make_path ?(hiera = false) e =

  let rec name t path = match t.desc with
      | Tlink t -> name t path
      | Tvar i -> begin match i with Some id -> id | None -> "'a" end
      | Tconstr (path, _, _) -> Path.name path
      | _ -> try List.assoc path !aliases with Not_found -> ""
  in

  let clas = match e.exp_desc with
    | Texp_send (e, Tmeth_name s, _)
    | Texp_send (e, Tmeth_val {name = s; _}, _) ->
        make_path ~hiera e ^ "#" ^ s

    | Texp_ident (path, _, typ) ->
        let path = Path.name path in
        if hiera then match typ.val_kind with
          | Val_self _ -> "self"
          | Val_anc _ -> "super"
          | _ ->
              if try List.assoc path !aliases = !last_class with Not_found -> false then
                "self"
              else ""
        else name typ.val_type path

    | Texp_instvar (_, path, _) -> name e.exp_type (Path.name path)

    | _ -> name e.exp_type ""
  in

  match e.exp_extra with
    | (Texp_coerce (_, typ), _, _)::_ ->
        let name = name typ.ctyp_type "" in
        let name =
          if List.mem name !defined then
            String.capitalize_ascii (unit !current_src) ^ "." ^ name
          else name
        in
        if List.mem clas !defined || String.contains clas '.' then
          if Hashtbl.mem content name then
            List.iter
              (fun (_, s) ->
                hashtbl_merge_list references (clas ^ "#" ^ s) references (name ^ "#" ^ s))
            (hashtbl_find_list content name)
          else
            treat_fields
              (fun s ->
                  hashtbl_merge_list references (clas ^ "#" ^ s) references (name ^ "#" ^ s)
              )
              typ.ctyp_type;
        name
    | _ -> clas



                (********   PROCESSING  ********)

let collect_export path u cltyp loc =

  let str = String.concat "." (List.tl (List.rev_map (fun id -> id.Ident.name) path)) in

  last_class := String.capitalize_ascii (unit !current_src) ^ "." ^ str;
  defined := str :: !defined;

  let save id =
    export ~sep:"#" path u (Ident.create id) loc;
    Hashtbl.replace
      content
      !last_class
      ((false, id) :: hashtbl_find_list content !last_class)
  in

  List.iter
    (fun (p, _) ->
      let name =
        let name = Path.name p in
        if List.mem name !defined then
          String.concat "." (List.rev_map (fun id -> id.Ident.name) (List.tl path)) ^ "." ^ name
        else name
      in
      Hashtbl.replace class_dependencies
        !last_class
        (name :: hashtbl_find_list class_dependencies !last_class))
    (sign cltyp).csig_inher;

  treat_fields save (sign cltyp).csig_self


let collect_references ?meth exp =

  let path = make_path exp in
  if path <> "" && path.[0] <> '#' then begin
    let path, meth = match meth with
      | Some s -> path ^ "#" ^ s, s
      | None ->
          let tmp = cut_sharp path 0 0 in
          let s = String.sub path (String.length tmp + 1) (String.length path - String.length tmp - 1) in
          path, s
    in
    let path =
      if List.mem (cut_sharp path 0 0) !defined then
        String.capitalize_ascii (unit !current_src)
        ^ "." ^ path
      else path
    in

    if exported path then begin
      Hashtbl.replace references path (exp.exp_loc :: hashtbl_find_list references path);
      let hiera = make_path ~hiera:true exp in
      if String.length hiera >= 4 && String.sub hiera 0 4 = "self" then
          Hashtbl.replace self_ref (!last_class ^ "#" ^ !last_field)
            ( let prev = hashtbl_find_list self_ref (!last_class ^ "#" ^ !last_field) in
              let e = (path, exp.exp_loc :: try List.assoc path prev with Not_found -> []) in
              e :: (List.remove_assoc path prev))
        else if String.length hiera >= 5 && String.sub hiera 0 5 = "super" then
          Hashtbl.replace super_ref (!last_class ^ "#" ^ meth) (path :: hashtbl_find_list super_ref (!last_class ^ "#" ^ meth))
   end
  end


let tstr ({ci_id_class_type=name; _}, _) =

  last_class :=
    String.capitalize_ascii (unit !current_src) ^ "."
    ^ String.concat "." (List.rev !mods)
    ^ (if !mods <> [] then "." ^ name.Ident.name else name.Ident.name)


let class_structure cl_struct =
  if Hashtbl.mem content !last_class then
    let cut_pat s = "self" ^ String.sub s 7 (String.length s - 7) in
    begin match cl_struct.cstr_self with
      | {pat_desc=Tpat_alias ({pat_desc=Tpat_alias ({pat_desc=Tpat_var (id, _); _}, id2, _); _}, id3, _); _} ->
          aliases :=
            (id.Ident.name, !last_class)
            :: (cut_pat id2.Ident.name, !last_class)
            :: (cut_pat id3.Ident.name, !last_class)
            :: !aliases
      | {pat_desc=Tpat_alias ({pat_desc=Tpat_alias (_, id1, _); _}, id2, _); } ->
          aliases :=
            (cut_pat id1.Ident.name, !last_class)
            :: (cut_pat id2.Ident.name, !last_class)
            :: !aliases
      | _ -> () end


let class_field f =

  let rec name cl_exp = match cl_exp.cl_desc with
    | Tcl_ident (path, _, _) -> Path.name path
    | Tcl_fun (_, _, _, cl_exp, _)
    | Tcl_apply (cl_exp, _)
    | Tcl_let (_, _, _, cl_exp)
    | Tcl_constraint (cl_exp, _, _, _, _) -> name cl_exp
    | _ -> ""
  in

  match f.cf_desc with
    | Tcf_inherit (_, cl_exp, s, _, l) ->
        let name = name cl_exp in
        let name =
        if List.mem name !defined then
          String.capitalize_ascii (unit !current_src) ^ "." ^ name
          else name
        in
        Hashtbl.replace class_dependencies !last_class
          (hashtbl_find_list class_dependencies !last_class @ [name]);
        begin match s with
          | Some s -> aliases := (s, name) :: !aliases
          | None -> () end;
        List.iter
          (fun (s, _) ->
            Hashtbl.replace content !last_class
              (List.map
                (fun (overr, f) -> if f = s then (false, f) else (overr, f))
                (hashtbl_find_list content !last_class)))
          l

    | Tcf_method ({txt; _}, _, _) ->
        Hashtbl.replace content !last_class
          (List.map
            (fun (overr, f) -> if f = txt then (true, f) else (overr, f))
              (hashtbl_find_list content !last_class));
        last_field := txt

    | _ -> ()


let rec arg typ args =
  match typ.desc with
  | Tlink t -> arg t args
  | Tarrow (_, t, typ, _) -> if args <> [] then begin arg t [(List.hd args)]; arg typ (List.tl args) end
  | Tobject _ ->
      treat_fields
        (fun s ->
          let _, e, _ = List.hd args in
          begin match e with
          | Some e ->
              collect_references ~meth:s e
          | None -> () end
        )
        typ
  | _ -> ()


let prepare_report () =

  let processed = ref [] in

  let rec process a l =
    if not (List.mem a !processed) then begin
      processed := a :: !processed;
      let met = ref [] in
      List.iter
        (fun c ->
          if not (List.mem c !processed) then
            process c (hashtbl_find_list class_dependencies c);
          List.iter
            (fun (_, f) ->
              let self, super = a ^ "#" ^ f, c ^ "#" ^ f in
              List.iter
                (fun path ->
                  process (cut_sharp path 0 0) (hashtbl_find_list class_dependencies (cut_sharp path 0 0));
                  List.iter
                    (fun (path, call_sites) ->
                      let src = cut_sharp path 0 0 in
                      let f = String.sub path (String.length src + 1) (String.length path - String.length src - 1) in
                      let overr, _ = try List.find (fun (_, s) -> s = f) (hashtbl_find_list content a) with Not_found -> (false, "") in
                      Hashtbl.replace references (a ^ "#" ^ f) (List.sort_uniq compare (call_sites @ hashtbl_find_list references (a ^ "#" ^ f)));
                      if not overr then
                        hashtbl_merge_list references (a ^ "#" ^ f) references path)
                    (hashtbl_find_list self_ref path))
                (hashtbl_find_list super_ref self);
              let overr, _ = try List.find (fun (_, s) -> s = f) (hashtbl_find_list content a) with Not_found -> (false, "") in
              if not (overr || List.mem f !met) then begin
                met := f :: !met;
                Hashtbl.iter
                  (fun _ l ->
                    List.iter
                      (fun (path, call_sites) ->
                        if path =  super then
                          Hashtbl.replace references (a ^ "#" ^ f) (List.sort_uniq compare (call_sites @ hashtbl_find_list references (a ^ "#" ^ f))))
                      l)
                  self_ref;
                List.iter
                  (fun (path, call_sites) -> 
                      let src = cut_sharp path 0 0 in
                      let f = String.sub path (String.length src + 1) (String.length path - String.length src - 1) in
                      Hashtbl.replace references (a ^ "#" ^ f) (List.sort_uniq compare (call_sites @ hashtbl_find_list references (a ^ "#" ^ f)))
                  )
                  (hashtbl_find_list self_ref super);
                hashtbl_merge_list references super references self;
                hashtbl_merge_list self_ref self self_ref super;
              end)
            (hashtbl_find_list content c))
        (List.rev l)
    end
  in

  Hashtbl.iter process class_dependencies
