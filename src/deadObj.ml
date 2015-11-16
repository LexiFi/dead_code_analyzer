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
          | _ when try List.assoc path !aliases = !last_class with Not_found -> false ->
              "self"
          |_ -> ""
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
                hashtbl_merge_list references (clas ^ "#" ^ s) references (name ^ "#" ^ s)
              )
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
    Hashtbl.replace content !last_class ((false, id) :: hashtbl_find_list content !last_class)
  in

  treat_fields save (sign cltyp).csig_self


let collect_references ?meth exp =

  let path = make_path exp in
  if path <> "" && path.[0] <> '#' && (String.contains path '#' || meth <> None) then begin
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
      hashtbl_add_to_list references path exp.exp_loc;
      let hiera = make_path ~hiera:true exp in
      if String.length hiera >= 4 && String.sub hiera 0 4 = "self" then
          Hashtbl.replace self_ref (!last_class ^ "#" ^ !last_field)
            ( let prev = hashtbl_find_list self_ref (!last_class ^ "#" ^ !last_field) in
              let e = (path, exp.exp_loc :: try List.assoc path prev with Not_found -> []) in
              e :: (List.remove_assoc path prev))
        else if String.length hiera >= 5 && String.sub hiera 0 5 = "super" then
          hashtbl_add_to_list super_ref (!last_class ^ "#" ^ meth) path
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

  let update_overr b s =
    let l = hashtbl_find_list content !last_class in
    let l =  List.map (fun ((_, f) as e) -> if f = s then (b, f) else e) l in
    Hashtbl.replace content !last_class l
  in

  match f.cf_desc with
    | Tcf_inherit (_, cl_exp, s, _, l) ->
        let name = name cl_exp in
        let name =
        if List.mem name !defined then
          String.capitalize_ascii (unit !current_src) ^ "." ^ name
          else name
        in
        hashtbl_add_to_list class_dependencies !last_class name;
        begin match s with
          | Some s -> aliases := (s, name) :: !aliases
          | None -> () end;
        List.iter
          (fun (s, _) ->
            update_overr false s)
          l

    | Tcf_method ({txt; _}, _, _) ->
        update_overr true txt;
        last_field := txt

    | _ -> ()


let rec arg typ args =
  match typ.desc with
  | Tlink t -> arg t args
  | Tarrow (_, t, typ, _) -> if args <> [] then begin arg t [(List.hd args)]; arg typ (List.tl args) end
  | Tobject _ ->
      treat_fields
        (fun s ->
          try
            let _, e, _ = List.hd args in
            begin match e with
            | Some e ->
                collect_references ~meth:s e
            | None -> () end
          with _ -> ()
        )
        typ
  | _ -> ()


let prepare_report () =

  let processed = Hashtbl.create 64 in

  let add_calls path call_sites =
    Hashtbl.replace references path (List.sort_uniq compare (call_sites @ hashtbl_find_list references path))
  in

  let rec process ?dep c =
    if not (Hashtbl.mem processed c) then begin
      Hashtbl.replace processed c ();
      let dep = match dep with
        | Some dep -> dep
        | None -> hashtbl_find_list class_dependencies c
      in
      let met = ref [] in
      List.iter
        (fun p -> process p; List.iter (fun e -> merge_refs e c p met) (hashtbl_find_list content p))
        dep
    end

  and merge_refs (_, f) c p met =
    let self, super = c ^ "#" ^ f, p ^ "#" ^ f in
    List.iter (fun path -> super_proc path c) (hashtbl_find_list super_ref self);
    let overr, _ =
      try List.find (fun (_, s) -> s = f) (hashtbl_find_list content c)
      with Not_found -> (false, "")
    in
    if not (overr || List.mem f !met) then begin
      met := f :: !met;
      Hashtbl.iter
        (fun _ l ->
          List.iter (fun (path, call_sites) -> if path =  super then add_calls (c ^ "#" ^ f) call_sites) l
        )
        self_ref;
      List.iter (fun e -> self_proc e c |> ignore) (hashtbl_find_list self_ref super);
      hashtbl_merge_list references super references self;
      hashtbl_merge_list self_ref self self_ref super
    end

  and super_proc path c =
    process (cut_sharp path 0 0);
    List.iter
      (fun (path, call_sites) ->
        let f = self_proc (path, call_sites) c in
        let overr, _ =
          try List.find (fun (_, s) -> s = f) (hashtbl_find_list content c)
          with Not_found -> (false, "")
        in
        if not overr then
          hashtbl_merge_list references (c ^ "#" ^ f) references path
      )
      (hashtbl_find_list self_ref path)

  and self_proc (path, call_sites) c =
    let src = cut_sharp path 0 0 in
    let f = String.sub path (String.length src + 1) (String.length path - String.length src - 1) in
    add_calls (c ^ "#" ^ f) call_sites;
    f

  in

  Hashtbl.iter (fun c dep -> process ~dep c) class_dependencies


let report () =
  prepare_report ();

  let rec report nb_call =
    let l =
      let folder = fun acc (fn, path, loc) ->
        let rec cut_main s pos =
          if pos = String.length s then s
          else if s.[pos] = '.' then String.sub s (pos + 1) (String.length s - pos - 1)
          else cut_main s (pos + 1)
        in
        if String.contains path '#' then match Hashtbl.find references path with
          | exception Not_found when nb_call = 0 ->
                (fn, cut_main path 0, loc, []) :: acc
          | exception Not_found -> acc
          | l when check_length nb_call l -> (fn, cut_main path 0, loc, l) :: acc
          | _ -> acc
        else acc
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
      if call_sites <> [] && !DeadFlag.obj.call_sites then print_string "    Call sites:";
      print_newline ();
      if !DeadFlag.obj.call_sites then begin
        List.iter (pretty_print_call ()) call_sites;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call = nb_call < !DeadFlag.obj.threshold in
    let s = if nb_call = 0 then "UNUSED CLASS FIELDS" else "ALMOST UNUSED CLASS FIELDS" in
    DeadCommon.report s l continue nb_call pretty_print report

  in report 0

