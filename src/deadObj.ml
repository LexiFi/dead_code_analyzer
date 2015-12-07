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

let decs = Hashtbl.create 256

let references = Hashtbl.create 512                     (* references by path#name *)

let self_ref = Hashtbl.create 512                       (* references by path#name to itself *)

let self_calls = Hashtbl.create 512

let super_ref = Hashtbl.create 512                      (* references by path#name to super *)

let content = Hashtbl.create 512                        (* classname -> [field names] *)

let inheritances = Hashtbl.create 512                   (* inheritance links *)

let dependencies = Hashtbl.create 512

let literals = ref []

let later = ref []

let defined = Hashtbl.create 32                         (* all classes defined in the current file *)

let aliases = Hashtbl.create 32                         (* aliases on class names *)

let last_class = ref _none                              (* last class met *)

let last_field = ref _none                              (* last field met *)



                (********   HELPERS   ********)


let rec sign = function
  | Cty_signature sg -> sg
  | Cty_arrow (_, _, t)
  | Cty_constr (_, _, t) -> sign t


let rec treat_fields action typ = match typ.desc with
  | Tobject (t, _)
  | Tarrow (_, _, t, _)
  | Tlink t -> treat_fields action t
  | Tfield (s, k, _, t) ->
      if Btype.field_kind_repr k = Fpresent then
        action s;
      treat_fields action t
  | _ -> ()


let full_name name =
  try
    Hashtbl.fold (fun _ (_, path) acc -> path::acc) incl []
    |> find_path name ~sep:'.'
  with Not_found ->
    let rec full_name intent name =
      try
        String.capitalize_ascii (unit !current_src) ^ "." ^ Hashtbl.find defined name
      with Not_found ->
        let cut = string_cut '.' name in
        let path = DeadMod.full_name cut in
        if intent = 0 && path <> cut then
          let name = (path ^ String.sub name (String.length cut) (String.length name - String.length cut)) in
          full_name 1 name
        else name
    in full_name 0 name


let rec make_name t path = match t.desc with
  | Tarrow (_, _, t, _)
  | Tlink t -> make_name t path
  | Tvar i -> begin match i with Some id -> id :: [] | None -> [] end
  | Tconstr (path, _, _) -> Path.name path :: []
  | _ -> hashtbl_find_list aliases path


let rec make_path ?(hiera = false) e =

  let classes = match e.exp_desc with
    | Texp_ident (path, _, typ) ->
        let path = Path.name path in
        if hiera then match typ.val_kind with
          | Val_self _ -> "self" :: []
          | Val_anc _ -> "super" :: []
          | _ when List.mem !last_class (hashtbl_find_list aliases path) ->
              "self" :: []
          | _ -> []
        else if List.mem (full_name (path ^ "*")) !literals then
          (path ^ "*")::[]
        else
          make_name e.exp_type path
    | Texp_new (path, _, _) -> Path.name path :: []

    | Texp_instvar (_, path, _) -> make_name e.exp_type (Path.name path)

    | _ as desc -> match make_name e.exp_type "" with
        | _obj :: [] -> begin match desc with
          | Texp_apply (e, _) -> make_path ~hiera e
          | _ -> [] end
        | l -> l
  in

  match e.exp_extra with
    | (Texp_coerce (_, typ), _, _)::_ ->
        let names = make_name typ.ctyp_type "" in
        let names = List.map full_name names
        in
        let merge name clas =
          if Hashtbl.mem defined clas || String.contains clas '.' then
            if Hashtbl.mem content name then
              List.iter
                (fun (_, s) ->
                  hashtbl_merge_unique_list references (clas ^ "#" ^ s) references (name ^ "#" ^ s)
                )
                (hashtbl_find_list content name)
            else
              treat_fields
                (fun s ->
                    hashtbl_merge_unique_list references (clas ^ "#" ^ s) references (name ^ "#" ^ s)
                )
                typ.ctyp_type
        in
        List.iter (fun name -> List.iter (merge name) classes) names;
        names
    | _ -> classes


let add_alias src dest =
  hashtbl_add_unique_to_list aliases src dest


let eom () =
  Hashtbl.reset defined;
  Hashtbl.reset aliases



                (********   PROCESSING  ********)


let collect_export path u stock ?obj ?cltyp loc =

  let str = String.concat "." (List.tl (List.rev_map (fun id -> id.Ident.name) path)) in
  Hashtbl.add defined str str;

  last_class := String.capitalize_ascii (unit !current_src) ^ "." ^ str;

  let stock =
    if stock != DeadCommon.decs then begin
      export (List.tl path) u stock (List.hd path) loc;
      stock
    end
    else
      decs
  in

  let save id =
    if stock == decs && not (Sys.file_exists (Filename.chop_extension !current_src ^ ".csml")) then
      export ~sep:"#" path u stock (Ident.create id) loc;
    hashtbl_add_unique_to_list content !last_class (false, id)
  in

  let typ = match cltyp with
    | None -> obj
    | Some cltyp -> Some (sign cltyp).csig_self
  in
  match typ with
    | Some typ -> treat_fields save typ
    | None -> ()


let collect_references ~meth ?path exp =

  let path = match path with
    | None -> make_path exp
    | Some path -> path
  in
  let process path =
    let path = full_name path ^ "#" ^ meth in
    hashtbl_add_unique_to_list references path exp.exp_loc;
    try
      let hiera = List.hd (make_path ~hiera:true exp) in
      if String.length hiera >= 4 && String.sub hiera 0 4 = "self" then begin
        let cur_path = !last_class ^ "#" ^ !last_field in
          hashtbl_replace_list self_ref cur_path
            ( let prev = hashtbl_find_list self_ref cur_path in
              let e = (path, exp.exp_loc :: try List.assoc path prev with Not_found -> []) in
              e :: (List.remove_assoc path prev));
          hashtbl_add_to_list self_calls path exp.exp_loc
      end
      else if String.length hiera >= 5 && String.sub hiera 0 5 = "super" then
        hashtbl_add_unique_to_list super_ref (!last_class ^ "#" ^ meth) path
    with _ -> ()
  in
  List.iter process path


let tstr ({ci_id_class_type=name; ci_expr; _}, _) =

  last_class :=
    String.concat "." (List.rev !mods)
    ^ (if !mods <> [] then "." ^ name.Ident.name else name.Ident.name);
  Hashtbl.add defined name.Ident.name !last_class;
  last_class :=
    String.capitalize_ascii (unit !current_src) ^ "." ^ !last_class;
  let add_depend p =
    let p = full_name (Path.name p) in
    hashtbl_add_unique_to_list dependencies !last_class p
  in
  let rec check_type = function
    | Cty_constr (p, _, _) -> add_depend p
    | Cty_signature _ -> ()
    | Cty_arrow (_, _, cl_type) -> check_type cl_type
  in
  check_type ci_expr.cl_type;
  let rec make_dep ci_expr =
    match ci_expr.cl_desc with
    | Tcl_ident (p, _, _) -> add_depend p
    | Tcl_fun (_, _, _, ci_expr, _)
    | Tcl_constraint (ci_expr, _, _, _, _) -> make_dep ci_expr
    | _ -> ()
  in
  make_dep ci_expr


let add_var name e =
  let rec is_obj e = match e.exp_desc with
    | Texp_object _ -> true
    | Texp_function (_, {c_rhs=e; _}::_, _) -> is_obj e
    | _ -> false
  in
  if is_obj e then begin
    Hashtbl.add defined name name;
    last_class := full_name name;
    literals := !last_class :: !literals
  end


let class_structure cl_struct =
  let cut_pat s =
    if String.length s > 7 && String.sub s 0 4 = "self" then
      "self" ^ String.sub s 7 (String.length s - 7)
    else s
  in
  let rec clean_sharp s p =
    try
      if s.[p] = '#' then String.sub s 0 p ^ String.sub s (p + 1) (String.length s - p - 1)
      else clean_sharp s (p + 1)
    with _ -> s
  in
  let rec add_aliases pat =
    begin match pat.pat_desc with
    | Tpat_alias (pat, id, _) -> add_alias (cut_pat id.Ident.name) !last_class; add_aliases pat
    | Tpat_var (id, _) -> add_alias id.Ident.name !last_class
    | _ -> () end;
    List.iter
      (function
        | (Tpat_constraint {ctyp_desc=Ttyp_class (p, _, _); _}, _, _) ->
            let name = clean_sharp (Path.name p) 0 in
            let name = full_name name in
            Hashtbl.iter (fun a e -> if e = !last_class then add_alias a name) aliases;
            hashtbl_add_unique_to_list dependencies !last_class name
        | _ -> ()
      )
      pat.pat_extra
  in
  add_aliases cl_struct.cstr_self


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
    let l = (b, s) :: List.filter (fun (_, f) -> f <> s) l in
    hashtbl_replace_list content !last_class l
  in

  match f.cf_desc with
    | Tcf_inherit (_, cl_exp, s, _, l) ->
        let name = name cl_exp in
        let name = full_name name in
        hashtbl_add_unique_to_list inheritances !last_class name;
        begin match s with
        | Some s -> add_alias s name
        | None -> () end;
        List.iter
          (fun (s, _) ->
            update_overr false s)
          l

    | Tcf_method ({txt; _}, _, _) ->
        update_overr true txt;
        last_field := txt

    | Tcf_constraint (t1, t2) ->
        let rec make_name t = match t.ctyp_desc with
          | Ttyp_arrow (_, _, t) -> make_name t
          | Ttyp_var id ->
              begin try Hashtbl.find aliases id with Not_found -> id end
          | Ttyp_class (path, _, _) ->
              let path = Path.name path in
              String.sub path 1 (String.length path - 1)
          | Ttyp_constr (path, _, _) ->
              let path = Path.name path in
              begin try Hashtbl.find aliases path with Not_found -> path end
          | _ -> "_"
        in
        let name1, name2 = make_name t1, make_name t2 in
        let name1, name2 = full_name name1, full_name name2 in
        hashtbl_add_unique_to_list dependencies name2 name1;
        hashtbl_add_unique_to_list dependencies name1 name2;

    | _ -> ()


let arg typ args =
  let rec arg self typ args =
    if args <> [] then match typ.desc with
      | Tlink t -> arg self t args
      | Tarrow (_, t, typ, _) when not self ->
          arg self t [(List.hd args)];
          if not self then
            arg self typ (List.tl args)
      | Tarrow (_, _, typ, _) ->
          arg self typ args
      | Tobject _ ->
          let _, e = List.hd args in
          begin match e with
          | Some e ->
              treat_fields (fun s -> collect_references ~meth:s e) typ
          | None -> () end
      | Tconstr (p, _, _) ->
          let _, e = List.hd args in
          begin match e with
          | Some e ->
              let p = full_name (Path.name p) in
              let path = make_path e in
              let collect () =
                List.iter
                  (fun (_, s) -> collect_references ~meth:s ~path e)
                  (hashtbl_find_list content p)
              in
              if path <> [] then
                if hashtbl_find_list content p = [] then
                  later := collect :: !later
                else collect ()
          | None -> () end
      | Tvar _ when not self->
          let _, e = List.hd args in
          begin match e with
          | Some e -> arg true e.exp_type args
          | _ -> () end
      | _ -> ()
  in arg false typ args


let prepare_report () =

  List.iter (fun x -> x()) !later;
  let processed = Hashtbl.create 64 in

  let add_calls path call_sites =
    List.iter (fun elt -> hashtbl_add_unique_to_list references path elt) call_sites;
  in

  let merge_eq m m2 =
    List.iter
      (fun c ->
        let c2 = m2 ^ "." ^ c in
        let c = m ^"." ^ c in
        if hashtbl_find_list content c <> [] then
          List.map (fun (_, f) -> (false, f)) (hashtbl_find_list content c)
          |> hashtbl_replace_list content c
        else
          List.map (fun (_, f) -> (false, f)) (hashtbl_find_list content c2)
          |> hashtbl_replace_list content c;
        hashtbl_replace_list inheritances c [c2];
      )
      (
        List.fold_left
          (fun acc (path, _) ->
            if path.[String.length path - 1] = '#' then
              String.sub path 0 (String.length path - 1) :: acc
            else acc
          )
          []
          (hashtbl_find_list DeadMod.content m)
      )
  in
  Hashtbl.iter merge_eq DeadMod.equal;

  let merge_dep c c2 =
    hashtbl_merge_unique_list inheritances c inheritances c2;
    List.iter
      (fun (b, s) ->
        hashtbl_find_list content c
        |> List.filter (fun (_, f) -> f <> s)
        |> hashtbl_replace_list content c;
        hashtbl_add_unique_to_list content c (b, s);
        let c = c ^ "#" ^ s in
        let c2 = c2 ^ "#" ^ s in
        hashtbl_merge_unique_list references (c) references (c2);
        hashtbl_merge_unique_list self_ref (c) self_ref (c2);
        hashtbl_merge_unique_list super_ref (c2) super_ref (c)
      )
      (hashtbl_find_list content c2)
  in
  Hashtbl.iter merge_dep dependencies;

  let rec process c =
    if not (Hashtbl.mem processed c) then begin
      Hashtbl.replace processed c ();
      let inher = hashtbl_find_list inheritances c in
      let met = ref [] in
      List.iter
        (fun p ->
          process p;
          List.iter (fun e -> merge_refs e c p met) (hashtbl_find_list content p)
        )
        inher;
    end

  and merge_refs (_, f) c p met =
    let self, super = c ^ "#" ^ f, p ^ "#" ^ f in
    List.iter (fun path -> super_proc path c) (hashtbl_find_list super_ref self);
    List.iter (fun e -> self_proc e c |> ignore) (hashtbl_find_list self_ref super);
    let overr, _ =
      try List.find (fun (_, s) -> s = f) (hashtbl_find_list content c)
      with Not_found -> (false, "")
    in
    if not (overr || List.mem f !met) then begin
      met := f :: !met;
      let loc = Hashtbl.fold (fun loc (_, path) acc -> if path = self then loc else acc) decs Location.none in
      let keep = hashtbl_find_list decs loc |> List.filter (fun (_, path) -> path <> self) in
      hashtbl_replace_list decs loc keep;
      hashtbl_merge_unique_list references super references self;
      hashtbl_merge_unique_list self_ref self self_ref super;
      hashtbl_merge_unique_list super_ref self super_ref super
    end;

  and super_proc path c =
    process (string_cut '#' path);
    List.iter
      (fun (p, call_sites) ->
        let f = self_proc (p, call_sites) c in
        let s =
          let src = string_cut '#' path in
          String.sub path (String.length src + 1) (String.length path - String.length src - 1)
        in
        hashtbl_add_unique_to_list self_ref (c ^ "#" ^ s) (c ^ "#" ^ f, call_sites)
      )
      (hashtbl_find_list self_ref path);

  and self_proc (path, call_sites) c =
    let src = string_cut '#' path in
    let f = String.sub path (String.length src + 1) (String.length path - String.length src - 1) in
    add_calls (c ^ "#" ^ f) call_sites;
    f

  in

  Hashtbl.iter (fun c _ -> process c) inheritances;
  Hashtbl.reset processed;

  let rec process c =
    if not (Hashtbl.mem processed c) then begin
      Hashtbl.replace processed c ();
      let inher = hashtbl_find_list inheritances c in
      let met = ref [] in
      List.iter
        (fun p ->
          process p;
          List.iter (fun e -> merge_refs e c p met) (hashtbl_find_list content p)
        )
        inher
    end

  and merge_refs (_, f) c p met =
    let self, super = c ^ "#" ^ f, p ^ "#" ^ f in
    let overr, _ =
      try List.find (fun (_, s) -> s = f) (hashtbl_find_list content c)
      with Not_found -> (false, "")
    in
    if not (overr || List.mem f !met) then begin
      met := f :: !met;
      hashtbl_merge_unique_list references super references self;
    end
  in

  Hashtbl.iter (fun c _ -> process c) inheritances;

  let merge_eq m m2 =
    List.iter
      (fun c ->
        let c2 = m2 ^ "." ^ c in
        let c = m ^"." ^ c in
        List.iter
          (fun (_, f) ->
            let e = c ^ "#" ^ f in
            let e2 = c2 ^ "#" ^ f in
            hashtbl_merge_unique_list references e references e2;
            hashtbl_replace_list references e2 (hashtbl_find_list references e)
          )
          (hashtbl_find_list content c)
      )
      (
        List.filter (fun (p, _) -> p.[String.length p - 1] = '#') (hashtbl_find_list DeadMod.content m)
        |> List.map (fun (path, _) -> String.sub path 0 (String.length path - 1))
      )
  in
  Hashtbl.iter merge_eq DeadMod.equal;
  Hashtbl.iter merge_eq DeadMod.equal


let report () =
  prepare_report ();

  let folder nb_call = fun loc (fn, path) acc ->
    let rec cut_main s pos =
      if pos = String.length s then s
      else if s.[pos] = '.' then String.sub s (pos + 1) (String.length s - pos - 1)
      else cut_main s (pos + 1)
    in
    match hashtbl_find_list references path with
      | exception Not_found when nb_call = 0 ->
            (fn, cut_main path 0, loc, []) :: acc
      | exception Not_found -> acc
      | l when check_length nb_call l -> (fn, cut_main path 0, loc, l) :: acc
      | _ -> acc
  in

  report_basic ~folder decs "UNUSED CLASS FIELDS" !DeadFlag.obj



                (********   WRAPPING  ********)


let wrap f x =
  if !DeadFlag.obj.print then f x else ()

let add_alias src dest =
  wrap (add_alias src) dest

let eom () =
  wrap eom ()

let collect_export path u stock ?obj ?cltyp loc =
  wrap (collect_export path u stock ?obj ?cltyp) loc

let collect_references ~meth ?path expr =
  wrap (collect_references ~meth ?path) expr

let tstr cl_dec =
  wrap tstr cl_dec

let add_var name expr =
  wrap (add_var name) expr

let class_structure cl_struct =
  wrap class_structure cl_struct

let class_field cl_field =
  wrap class_field cl_field

let arg typ args =
  wrap (arg typ) args

let report () =
  wrap report ()
