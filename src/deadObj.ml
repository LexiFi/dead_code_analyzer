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

let references = Hashtbl.create 32                     (* references by path#name *)

let self_ref = Hashtbl.create 32                       (* references by path#name to itself *)

let super_ref = Hashtbl.create 32                      (* references by path#name to super *)

let content = Hashtbl.create 32                        (* classname -> [field names] *)

let inheritances = Hashtbl.create 32                   (* inheritance links *)

let dependencies = Hashtbl.create 32

let literals = ref []

let later = ref []



let defined = ref []                                    (* all classes defined in the current file *)

let aliases = ref []                                    (* aliases on class names *)

let last_class = ref "*none*"                           (* last class met *)

let last_field = ref "*none*"                           (* last field met *)



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
    List.map (fun (_, path, _) -> string_cut '#' path) !incl
    |> find_path name ~sep:'.'
  with Not_found ->
    if List.mem name !defined then
      String.capitalize_ascii (unit !current_src) ^ "." ^ name
    else name

let rec make_name t path = match t.desc with
  | Tarrow (_, _, t, _)
  | Tlink t -> make_name t path
  | Tvar i -> begin match i with Some id -> id :: [] | None -> [] end
  | Tconstr (path, _, _) -> Path.name path :: []
  | _ -> try List.assoc path !aliases with Not_found -> []


let rec make_path ?(hiera = false) e =

  let classes = match e.exp_desc with
    | Texp_send (e, Tmeth_name s, _)
    | Texp_send (e, Tmeth_val {name = s; _}, _) ->
        List.map (fun c -> c ^ "#" ^ s) (make_path ~hiera e)

    | Texp_ident (path, _, typ) ->
        let path = Path.name path in
        if hiera then match typ.val_kind with
          | Val_self _ -> "self" :: []
          | Val_anc _ -> "super" :: []
          | _ when try List.mem !last_class (List.assoc path !aliases) with Not_found -> false ->
              "self" :: []
          | _ -> []
        else if List.mem (full_name (path ^ "*")) !literals then
          (path ^ "*")::[]
        else
          make_name e.exp_type path

    | Texp_instvar (_, path, _) -> make_name e.exp_type (Path.name path)

    | _ as desc -> match make_name e.exp_type "" with
        | "*obj*" :: [] -> begin match desc with
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
                typ.ctyp_type
        in
        List.iter (fun name -> List.iter (merge name) classes) names;
        names
    | _ -> classes



                (********   PROCESSING  ********)

let collect_export path u stock ?obj ?cltyp loc =

  let str = String.concat "." (List.tl (List.rev_map (fun id -> id.Ident.name) path)) in

  last_class := String.capitalize_ascii (unit !current_src) ^ "." ^ str;
  defined := str :: !defined;

  let save id =
    export ~sep:"#" path u stock (Ident.create id) loc;
    hashtbl_add_to_list content !last_class (false, id)
  in

  let typ = match cltyp with
    | None -> obj
    | Some cltyp -> Some (sign cltyp).csig_self
  in
  match typ with
    | Some typ -> treat_fields save typ
    | None -> ()


let collect_references ?meth exp =

  let path = make_path exp in
  let process path =
    if path <> "" && path.[0] <> '#' && (String.contains path '#' || meth <> None) then begin
      let path, meth = match meth with
        | Some s -> path, s
        | None ->
            let tmp = string_cut '#' path in
            let s = String.sub path (String.length tmp + 1) (String.length path - String.length tmp - 1) in
            tmp, s
      in
      let path = full_name path ^ "#" ^ meth in

      if exported path then begin
        hashtbl_add_to_list references path exp.exp_loc;
        try
          let hiera = List.hd (make_path ~hiera:true exp) in
          if String.length hiera >= 4 && String.sub hiera 0 4 = "self" then
              Hashtbl.replace self_ref (!last_class ^ "#" ^ !last_field)
                ( let prev = hashtbl_find_list self_ref (!last_class ^ "#" ^ !last_field) in
                  let e = (path, exp.exp_loc :: try List.assoc path prev with Not_found -> []) in
                  e :: (List.remove_assoc path prev))
            else if String.length hiera >= 5 && String.sub hiera 0 5 = "super" then
              hashtbl_add_to_list super_ref (!last_class ^ "#" ^ meth) path
        with _ -> ()
     end
    end
  in
  List.iter process path


let tstr ({ci_id_class_type=name; ci_expr; _}, _) =

  last_class :=
    String.capitalize_ascii (unit !current_src) ^ "."
    ^ String.concat "." (List.rev !mods)
    ^ (if !mods <> [] then "." ^ name.Ident.name else name.Ident.name);
  let rec make_dep ci_expr = match ci_expr.cl_desc with
    | Tcl_ident (p, _, _) ->
        let p = full_name (Path.name p) in
        let name = full_name name.Ident.name in
        hashtbl_add_to_list dependencies name p;
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
    defined := name :: !defined;
    last_class := full_name name;
    literals := !last_class :: !literals
  end

let class_structure cl_struct =
  if Hashtbl.mem content !last_class then
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
    let rec add_alias pat = begin match pat.pat_desc with
      | Tpat_alias (pat, id, _) -> aliases := (cut_pat id.Ident.name, !last_class :: []) :: !aliases; add_alias pat
      | Tpat_var (id, _) -> aliases := (id.Ident.name, !last_class :: []) :: !aliases
      | _ -> () end;
      List.iter
        (function
          | (Tpat_constraint {ctyp_desc=Ttyp_class (p, _, _); _}, _, _) ->
              let name = clean_sharp (Path.name p) 0 in
              let name = full_name name in
              aliases := List.map (fun (a, l) -> if List.mem !last_class l then (a, name::l) else (a, l)) !aliases
          | _ -> ()
        )
        pat.pat_extra
    in
    add_alias cl_struct.cstr_self


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
    let l =
      if List.exists (fun (_, f) -> f = s) l then
        List.map (fun ((_, f) as e) -> if f = s then (b, f) else e) l
      else (b, s) :: l
    in
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
        hashtbl_add_to_list inheritances !last_class name;
        begin match s with
          | Some s -> aliases := (s, name::[]) :: !aliases
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
      begin try
        let _, e, _ = List.hd args in
        begin match e with
        | Some e ->
            treat_fields (fun s -> collect_references ~meth:s e) typ
        | None -> () end
      with _ -> () end
  | Tconstr (p, _, _) ->
      let p = full_name (Path.name p) in
      let collect () =
        try
          let _, e, _ = List.hd args in
          begin match e with
          | Some e ->
              List.iter (fun (_, s) -> collect_references ~meth:s e) (hashtbl_find_list content p)
          | None -> () end
        with _ -> ()
      in
      later := collect :: !later
  | _ -> ()


let prepare_report () =

  List.iter (fun x -> x()) !later;
  let processed = Hashtbl.create 64 in

  let add_calls path call_sites =
    Hashtbl.replace references path (List.sort_uniq compare (call_sites @ hashtbl_find_list references path))
  in

  let merge_eq m eq =
    List.iter
      (fun c ->
        let c2 = c in
        let c = m ^"." ^c in
        List.iter
          (fun m2 ->
            let c2 = m2 ^ "." ^ c2 in
            List.map (fun (_, f) -> (false, f)) (hashtbl_find_list content c)
            |> Hashtbl.replace content c;
            Hashtbl.replace inheritances c [c2];
          )
          eq
      )
      (
        List.filter (fun (p, _) -> p.[String.length p - 1] = '#') (hashtbl_find_list DeadMod.content m)
        |> List.map (fun (path, _) -> String.sub path 0 (String.length path - 1))
      )
  in
  Hashtbl.iter merge_eq DeadMod.equal;

  let merge_dep c dep =
    List.iter
      (fun c2 ->
        hashtbl_merge_list inheritances c inheritances c2;
        List.iter
          (fun (b, s) ->
            hashtbl_find_list content c
            |> List.filter (fun (_, f) -> f <> s)
            |> Hashtbl.replace content c;
            hashtbl_add_to_list content c (b, s);
            hashtbl_merge_list inheritances c inheritances c2;
            hashtbl_merge_list self_ref (c ^ "#" ^ s) self_ref (c2 ^ "#" ^ s);
            hashtbl_merge_list super_ref (c2 ^ "#" ^ s) super_ref (c ^ "#" ^ s)
          )
          (hashtbl_find_list content c2)
      )
      dep
  in
  Hashtbl.iter merge_dep dependencies;

  let rec process ?inher c =
    if not (Hashtbl.mem processed c) then begin
      Hashtbl.replace processed c ();
      let inher = match inher with
        | Some inher -> inher
        | None -> hashtbl_find_list inheritances c
      in
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
      Hashtbl.iter
        (fun _ l ->
          List.iter (fun (path, call_sites) -> if path =  super then add_calls (c ^ "#" ^ f) call_sites) l
        )
        self_ref;
      hashtbl_merge_list references super references self;
      Hashtbl.replace references self (hashtbl_find_list references super);
      hashtbl_merge_list self_ref self self_ref super;
      hashtbl_merge_list super_ref self super_ref super
    end

  and super_proc path c =
    process (string_cut '#' path);
    List.iter
      (fun (p, call_sites) ->
        let f = self_proc (p, call_sites) c in
        let overr, _ =
          try List.find (fun (_, s) -> s = f) (hashtbl_find_list content c)
          with Not_found -> (false, "")
        in
        if not overr then
          hashtbl_merge_list references (c ^ "#" ^ f) references p;
        let s =
          let src = string_cut '#' path in
          String.sub path (String.length src + 1) (String.length path - String.length src - 1)
        in
        hashtbl_add_to_list self_ref (c ^ "#" ^ s) (c ^ "#" ^ f, call_sites)
      )
      (hashtbl_find_list self_ref path)

  and self_proc (path, call_sites) c =
    let src = string_cut '#' path in
    let f = String.sub path (String.length src + 1) (String.length path - String.length src - 1) in
    add_calls (c ^ "#" ^ f) call_sites;
    f

  in

  Hashtbl.iter (fun c inher -> process ~inher c) inheritances;
  Hashtbl.reset processed;

  let rec process ?inher c =
    if not (Hashtbl.mem processed c) then begin
      Hashtbl.replace processed c ();
      let inher = match inher with
        | Some inher -> inher
        | None -> hashtbl_find_list inheritances c
      in
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
      Hashtbl.iter
        (fun _ l ->
          List.iter (fun (path, call_sites) -> if path =  super then add_calls (c ^ "#" ^ f) call_sites) l
        )
        self_ref;
      hashtbl_merge_list references super references self;
      Hashtbl.replace references self (hashtbl_find_list references super);
    end
  in

  Hashtbl.iter (fun c inher -> process ~inher c) inheritances;

  let merge_eq m eq =
    List.iter
      (fun e ->
        let c = string_cut '#' e in
        let c, f =
          String.sub c (String.length m + 1) (String.length c - String.length m - 1),
          String.sub e (String.length c + 1) (String.length e - String.length c - 1)
        in
        List.iter
          (fun m2 ->
            let e2 = m2 ^ "." ^ c ^ "#" ^ f in
            hashtbl_merge_list references e2 references e;
            hashtbl_merge_list references e references e2;
          )
          eq
      )
      (
        List.map (fun (_, path, _) -> path) !decs
        |> List.filter
          (fun path ->
            if String.length path > String.length m && String.contains path '#'
            && String.sub path 0 (String.length m) = m then begin
            true
            end
            else false
          )
      )
  in
  Hashtbl.iter merge_eq DeadMod.equal


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

