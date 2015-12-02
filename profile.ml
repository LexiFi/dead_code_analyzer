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

let last_class = ref "*none*"                           (* last class met *)

let last_field = ref "*none*"                           (* last field met *)



                (********   HELPERS   ********)


let rec sign = function
  | Cty_signature sg -> (* 305 *) sg
  | Cty_arrow (_, _, t)
  | Cty_constr (_, _, t) -> (* 70 *) sign t


let rec treat_fields action typ = (* 77639 *) match typ.desc with
  | Tobject (t, _)
  | Tarrow (_, _, t, _)
  | Tlink t -> (* 27281 *) treat_fields action t
  | Tfield (s, k, _, t) ->
      (* 31776 *) if Btype.field_kind_repr k = Fpresent then
        (* 31461 *) action s;
      treat_fields action t
  | _ -> (* 18582 *) ()


let full_name name =
  (* 564564 *) try
    List.map (fun (_, path, _) -> (* 0 *) path) !incl
    |> find_path name ~sep:'.'
  with Not_found ->
    (* 564564 *) let rec full_name intent name =
      (* 595162 *) try
        String.capitalize_ascii (unit !current_src) ^ "." ^ Hashtbl.find defined name
      with Not_found ->
        (* 595070 *) let cut = string_cut '.' name in
        let path = DeadMod.full_name cut in
        if intent = 0 && path <> cut then
          (* 30598 *) let name = (path ^ String.sub name (String.length cut) (String.length name - String.length cut)) in
          full_name 1 name
        else (* 564472 *) name
    in full_name 0 name


let rec make_name t path = (* 3959771 *) match t.desc with
  | Tarrow (_, _, t, _)
  | Tlink t -> (* 3739731 *) make_name t path
  | Tvar i -> (* 209 *) begin match i with Some id -> (* 10 *) id :: [] | None -> (* 199 *) [] end
  | Tconstr (path, _, _) -> (* 217675 *) Path.name path :: []
  | _ -> (* 2156 *) hashtbl_find_list aliases path


let rec make_path ?(hiera = false) e =

  (* 220425 *) let classes = match e.exp_desc with
    | Texp_ident (path, _, typ) ->
        (* 128827 *) let path = Path.name path in
        if hiera then (* 382 *) match typ.val_kind with
          | Val_self _ -> (* 182 *) "self" :: []
          | Val_anc _ -> (* 51 *) "super" :: []
          | _ when (* 149 *) List.mem !last_class (hashtbl_find_list aliases path) ->
              (* 63 *) "self" :: []
          | _ -> (* 86 *) []
        else (* 128445 *) if List.mem (full_name (path ^ "*")) !literals then
          (* 0 *) (path ^ "*")::[]
        else
          (* 128445 *) make_name e.exp_type path
    | Texp_new (path, _, _) -> (* 10 *) Path.name path :: []

    | Texp_instvar (_, path, _) -> (* 9 *) make_name e.exp_type (Path.name path)

    | _ as desc -> (* 91579 *) match make_name e.exp_type "" with
        | "*obj*" :: [] -> (* 0 *) begin match desc with
          | Texp_apply (e, _) -> (* 0 *) make_path ~hiera e
          | _ -> (* 0 *) [] end
        | l -> (* 91579 *) l
  in

  match e.exp_extra with
    | (Texp_coerce (_, typ), _, _)::_ ->
        (* 7 *) let names = make_name typ.ctyp_type "" in
        let names = List.map full_name names
        in
        let merge name clas =
          (* 7 *) if Hashtbl.mem defined clas || String.contains clas '.' then
            (* 7 *) if Hashtbl.mem content name then
              (* 0 *) List.iter
                (fun (_, s) ->
                  (* 0 *) hashtbl_merge_unique_list references (clas ^ "#" ^ s) references (name ^ "#" ^ s)
                )
                (hashtbl_find_list content name)
            else
              (* 7 *) treat_fields
                (fun s ->
                    (* 0 *) hashtbl_merge_unique_list references (clas ^ "#" ^ s) references (name ^ "#" ^ s)
                )
                typ.ctyp_type
        in
        List.iter (fun name -> (* 7 *) List.iter (merge name) classes) names;
        names
    | _ -> (* 220418 *) classes


let add_alias src dest =
  (* 77 *) hashtbl_add_unique_to_list aliases src dest


let eom () =
  (* 697 *) Hashtbl.reset defined;
  Hashtbl.reset aliases



                (********   PROCESSING  ********)


let collect_export path u stock ?obj ?cltyp loc =

  (* 18447 *) let str = String.concat "." (List.tl (List.rev_map (fun id -> (* 44762 *) id.Ident.name) path)) in

  last_class := String.capitalize_ascii (unit !current_src) ^ "." ^ str;

  if stock != decs then
    (* 12 *) export (List.tl path) u stock (List.hd path) loc;

  let save id =
    (* 31238 *) if stock == decs && not (Sys.file_exists (Filename.chop_extension !current_src ^ ".csml")) then
      (* 30866 *) export ~sep:"#" path u stock (Ident.create id) loc;
    hashtbl_add_unique_to_list content !last_class (false, id)
  in

  let typ = match cltyp with
    | None -> (* 18142 *) obj
    | Some cltyp -> (* 305 *) Some (sign cltyp).csig_self
  in
  match typ with
    | Some typ -> (* 18447 *) treat_fields save typ
    | None -> (* 0 *) ()


let collect_references ?meth ?path exp =

  (* 1900 *) let path = match path with
    | None -> (* 1900 *) make_path exp
    | Some path -> (* 0 *) path
  in
  let process path =
    (* 398 *) if path <> "" && path.[0] <> '#' && (String.contains path '#' || meth <> None) then (* 398 *) begin
      let path, meth = match meth with
        | Some s -> (* 398 *) path, s
        | None ->
            (* 0 *) let tmp = string_cut '#' path in
            let s = String.sub path (String.length tmp + 1) (String.length path - String.length tmp - 1) in
            tmp, s
      in
      let path = full_name path ^ "#" ^ meth in

      hashtbl_add_unique_to_list references path exp.exp_loc;
      try
        let hiera = List.hd (make_path ~hiera:true exp) in
        if String.length hiera >= 4 && String.sub hiera 0 4 = "self" then (* 245 *) begin
          let cur_path = !last_class ^ "#" ^ !last_field in
            hashtbl_replace_list self_ref cur_path
              ( let prev = hashtbl_find_list self_ref cur_path in
                let e = (path, exp.exp_loc :: try List.assoc path prev with Not_found -> (* 131 *) []) in
                e :: (List.remove_assoc path prev));
            hashtbl_add_to_list self_calls path exp.exp_loc
        end
        else (* 67 *) if String.length hiera >= 5 && String.sub hiera 0 5 = "super" then
          (* 51 *) hashtbl_add_unique_to_list super_ref (!last_class ^ "#" ^ meth) path
      with _ -> (* 86 *) ()
    end
  in
  List.iter process path


let tstr ({ci_id_class_type=name; ci_expr; _}, _) =

  (* 23 *) last_class :=
    String.concat "." (List.rev !mods)
    ^ (if !mods <> [] then (* 3 *) "." ^ name.Ident.name else (* 20 *) name.Ident.name);
  Hashtbl.add defined name.Ident.name !last_class;
  last_class :=
    String.capitalize_ascii (unit !current_src) ^ "." ^ !last_class;
  let add_depend p =
    (* 1 *) let p = full_name (Path.name p) in
    hashtbl_add_unique_to_list dependencies !last_class p
  in
  let rec check_type = function
    | Cty_constr (p, _, _) -> (* 1 *) add_depend p
    | Cty_signature _ -> (* 22 *) ()
    | Cty_arrow (_, _, cl_type) -> (* 17 *) check_type cl_type
  in
  check_type ci_expr.cl_type;
  let rec make_dep ci_expr =
    (* 35 *) match ci_expr.cl_desc with
    | Tcl_ident (p, _, _) -> (* 0 *) add_depend p
    | Tcl_fun (_, _, _, ci_expr, _)
    | Tcl_constraint (ci_expr, _, _, _, _) -> (* 12 *) make_dep ci_expr
    | _ -> (* 23 *) ()
  in
  make_dep ci_expr


let add_var name e =
  (* 59700 *) let rec is_obj e = (* 107534 *) match e.exp_desc with
    | Texp_object _ -> (* 1 *) true
    | Texp_function (_, {c_rhs=e; _}::_, _) -> (* 47834 *) is_obj e
    | _ -> (* 59699 *) false
  in
  if is_obj e then (* 1 *) begin
    Hashtbl.add defined name name;
    last_class := full_name name;
    literals := !last_class :: !literals
  end


let class_structure cl_struct =
  (* 24 *) let cut_pat s =
    (* 49 *) if String.length s > 7 && String.sub s 0 4 = "self" then
      (* 48 *) "self" ^ String.sub s 7 (String.length s - 7)
    else (* 1 *) s
  in
  let rec clean_sharp s p =
    (* 0 *) try
      if s.[p] = '#' then (* 0 *) String.sub s 0 p ^ String.sub s (p + 1) (String.length s - p - 1)
      else (* 0 *) clean_sharp s (p + 1)
    with _ -> (* 0 *) s
  in
  let rec add_aliases pat =
    (* 73 *) begin match pat.pat_desc with
    | Tpat_alias (pat, id, _) -> (* 49 *) add_alias (cut_pat id.Ident.name) !last_class; add_aliases pat
    | Tpat_var (id, _) -> (* 11 *) add_alias id.Ident.name !last_class
    | _ -> (* 13 *) () end;
    List.iter
      (function
        | (Tpat_constraint {ctyp_desc=Ttyp_class (p, _, _); _}, _, _) ->
            (* 0 *) let name = clean_sharp (Path.name p) 0 in
            let name = full_name name in
            Hashtbl.iter (fun a e -> (* 0 *) if e = !last_class then (* 0 *) add_alias a name) aliases;
            hashtbl_add_unique_to_list dependencies !last_class name
        | _ -> (* 1 *) ()
      )
      pat.pat_extra
  in
  add_aliases cl_struct.cstr_self


let class_field f =

  (* 141 *) let rec name cl_exp = (* 30 *) match cl_exp.cl_desc with
    | Tcl_ident (path, _, _) -> (* 15 *) Path.name path
    | Tcl_fun (_, _, _, cl_exp, _)
    | Tcl_apply (cl_exp, _)
    | Tcl_let (_, _, _, cl_exp)
    | Tcl_constraint (cl_exp, _, _, _, _) -> (* 15 *) name cl_exp
    | _ -> (* 0 *) ""
  in

  let update_overr b s =
    (* 317 *) let l = hashtbl_find_list content !last_class in
    let l =
      if List.exists (fun (_, f) -> (* 2768 *) f = s) l then
        (* 196 *) List.map (fun ((_, f) as e) -> (* 3289 *) if f = s then (* 196 *) (b, f) else (* 3093 *) e) l
      else (* 121 *) (b, s) :: l
    in
    hashtbl_replace_list content !last_class l
  in

  match f.cf_desc with
    | Tcf_inherit (_, cl_exp, s, _, l) ->
        (* 15 *) let name = name cl_exp in
        let name = full_name name in
        hashtbl_add_unique_to_list inheritances !last_class name;
        begin match s with
          | Some s -> (* 14 *) add_alias s name
          | None -> (* 1 *) () end;
        List.iter
          (fun (s, _) ->
            (* 208 *) update_overr false s)
          l

    | Tcf_method ({txt; _}, _, _) ->
        (* 109 *) update_overr true txt;
        last_field := txt

    | Tcf_constraint (t1, t2) ->
        (* 0 *) let rec make_name t = (* 0 *) match t.ctyp_desc with
          | Ttyp_arrow (_, _, t) -> (* 0 *) make_name t
          | Ttyp_var id ->
              (* 0 *) begin try Hashtbl.find aliases id with Not_found -> (* 0 *) id end
          | Ttyp_class (path, _, _) ->
              (* 0 *) let path = Path.name path in
              String.sub path 1 (String.length path - 1)
          | Ttyp_constr (path, _, _) ->
              (* 0 *) let path = Path.name path in
              begin try Hashtbl.find aliases path with Not_found -> (* 0 *) path end
          | _ -> (* 0 *) "_"
        in
        let name1, name2 = make_name t1, make_name t2 in
        let name1, name2 = full_name name1, full_name name2 in
        hashtbl_add_unique_to_list dependencies name2 name1;
        hashtbl_add_unique_to_list dependencies name1 name2;

    | _ -> (* 17 *) ()


let arg typ args = (* 126508 *) let rec arg self typ args = (* 10187121 *) match typ.desc with
  | Tlink t -> (* 9537963 *) arg self t args
  | Tarrow (_, t, typ, _) when (* 263640 *) args <> [] && not self ->
      (* 246291 *) arg self t [(List.hd args)];
      arg self typ (List.tl args)
  | Tarrow (_, _, typ, _) when (* 17349 *) self ->
      (* 8438 *) arg self typ args
  | Tobject _ ->
      (* 136 *) begin try
        let _, e, _ = List.hd args in
        begin match e with
        | Some e ->
            (* 128 *) treat_fields (fun s -> (* 223 *) collect_references ~meth:s e) typ
        | None -> (* 0 *) () end
      with _ -> (* 8 *) () end
  | Tconstr (p, _, _) ->
      (* 323206 *) begin try
        let _, e, _ = List.hd args in
        match e with
        | Some e ->
            (* 218127 *) let p = full_name (Path.name p) in
            let path =
              make_path e
              |> List.map full_name
              |> List.filter (fun path -> (* 217570 *) path = String.capitalize_ascii path)
            in
            if path <> [] && hashtbl_find_list content p <> [] then
              (* 0 *) let collect () =
                (* 0 *) List.iter
                  (fun (_, s) -> (* 0 *) collect_references ~meth:s ~path e)
                  (hashtbl_find_list content p)
              in
              later := collect :: !later
        | None -> (* 10 *) ()
      with _ -> (* 105069 *) () end
  | Tvar _ when (* 45541 *) not self->
      (* 36867 *) begin try
        let _, e, _ = List.hd args in
        match e with
        | Some e -> (* 21630 *) arg true e.exp_type args
        | _ -> (* 0 *) ()
      with _ -> (* 15237 *) () end
  | _ -> (* 34220 *) ()
  in arg false typ args


let prepare_report () =
  (* 1 *) let prerr_check ()=
    (* 403 *) prerr_endline ("CHECK:      " ^ (string_of_float @@  Sys.time ())) in

  prerr_check();
  List.iter (fun x -> (* 0 *) x()) !later;
  let processed = Hashtbl.create 64 in

  let add_calls path call_sites =
    (* 1302 *) List.iter (fun elt -> (* 3816 *) hashtbl_add_unique_to_list references path elt) call_sites;
  in

  let merge_eq m m2 =
    (* 533 *) List.iter
      (fun c ->
        (* 4 *) let c2 = m2 ^ "." ^ c in
        let c = m ^"." ^ c in
        if hashtbl_find_list content c <> [] then
          (* 2 *) List.map (fun (_, f) -> (* 8 *) (false, f)) (hashtbl_find_list content c)
          |> hashtbl_replace_list content c
        else
          (* 2 *) List.map (fun (_, f) -> (* 8 *) (false, f)) (hashtbl_find_list content c2)
          |> hashtbl_replace_list content c;
        hashtbl_replace_list inheritances c [c2];
      )
      (
        List.fold_left
          (fun acc (path, _) ->
            (* 8441 *) if path.[String.length path - 1] = '#' then
              (* 4 *) String.sub path 0 (String.length path - 1) :: acc
            else (* 8437 *) acc
          )
          []
          (hashtbl_find_list DeadMod.content m)
      )
  in
  Hashtbl.iter merge_eq DeadMod.equal;

  let merge_dep c c2 =
    (* 1 *) hashtbl_merge_unique_list inheritances c inheritances c2;
    List.iter
      (fun (b, s) ->
        (* 0 *) hashtbl_find_list content c
        |> List.filter (fun (_, f) -> (* 0 *) f <> s)
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
  prerr_check();

  let rec process c =
    (* 83 *) if not (Hashtbl.mem processed c) then (* 20 *) begin
      Hashtbl.replace processed c ();
      let inher = hashtbl_find_list inheritances c in
      let met = ref [] in
      List.iter
        (fun p ->
          (* 17 *) process p;
          List.iter (fun e -> (* 232 *) merge_refs e c p met) (hashtbl_find_list content p)
        )
        inher;
    end

  and merge_refs (_, f) c p met =
    (* 232 *) let self, super = c ^ "#" ^ f, p ^ "#" ^ f in
    List.iter (fun path -> (* 49 *) super_proc path c) (hashtbl_find_list super_ref self);
    List.iter (fun e -> (* 585 *) self_proc e c |> ignore) (hashtbl_find_list self_ref super);
    let overr, _ =
      try List.find (fun (_, s) -> (* 2170 *) s = f) (hashtbl_find_list content c)
      with Not_found -> (* 16 *) (false, "")
    in
    if not (overr || List.mem f !met) then (* 192 *) begin
      met := f :: !met;
    prerr_check();
    add_calls self (hashtbl_find_list self_calls super);
    prerr_check();
      decs := List.filter (fun (_, path, _) -> (* 3878964 *) path <> self) !decs;
      hashtbl_merge_unique_list references super references self;
      hashtbl_replace_list references self (hashtbl_find_list references super);
      hashtbl_merge_unique_list self_ref self self_ref super;
      hashtbl_merge_unique_list super_ref self super_ref super
    end;

  and super_proc path c =
    (* 49 *) process (string_cut '#' path);
    List.iter
      (fun (p, call_sites) ->
        (* 333 *) let f = self_proc (p, call_sites) c in
        let overr, _ =
          try List.find (fun (_, s) -> (* 3696 *) s = f) (hashtbl_find_list content c)
          with Not_found -> (* 33 *) (false, "")
        in
        if not overr then
          (* 241 *) hashtbl_merge_unique_list references (c ^ "#" ^ f) references p;
        let s =
          let src = string_cut '#' path in
          String.sub path (String.length src + 1) (String.length path - String.length src - 1)
        in
        hashtbl_add_unique_to_list self_ref (c ^ "#" ^ s) (c ^ "#" ^ f, call_sites)
      )
      (hashtbl_find_list self_ref path);

  and self_proc (path, call_sites) c =
    (* 918 *) let src = string_cut '#' path in
    let f = String.sub path (String.length src + 1) (String.length path - String.length src - 1) in
    add_calls (c ^ "#" ^ f) call_sites;
    f

  in

  Hashtbl.iter (fun c _ -> (* 17 *) prerr_check(); process c) inheritances;
  Hashtbl.reset processed;

  let rec process c =
    (* 34 *) if not (Hashtbl.mem processed c) then (* 20 *) begin
      Hashtbl.replace processed c ();
      let inher = hashtbl_find_list inheritances c in
      let met = ref [] in
      List.iter
        (fun p ->
          (* 17 *) process p;
          List.iter (fun e -> (* 232 *) merge_refs e c p met) (hashtbl_find_list content p)
        )
        inher
    end

  and merge_refs (_, f) c p met =
    (* 232 *) let self, super = c ^ "#" ^ f, p ^ "#" ^ f in
    let overr, _ =
      try List.find (fun (_, s) -> (* 2170 *) s = f) (hashtbl_find_list content c)
      with Not_found -> (* 16 *) (false, "")
    in
    if not (overr || List.mem f !met) then (* 192 *) begin
      met := f :: !met;
      add_calls self (hashtbl_find_list self_calls super);
      hashtbl_merge_unique_list references super references self;
      hashtbl_replace_list references self (hashtbl_find_list references super);
    end
  in

  Hashtbl.iter (fun c _ -> (* 17 *) process c) inheritances;

  let merge_eq m m2 =
    (* 1066 *) List.iter
      (fun c ->
        (* 8 *) let c2 = m2 ^ "." ^ c in
        let c = m ^"." ^ c in
        List.iter
          (fun (_, f) ->
            (* 32 *) let e = c ^ "#" ^ f in
            let e2 = c2 ^ "#" ^ f in
            hashtbl_merge_unique_list references e references e2;
            hashtbl_merge_unique_list references e2 references e
          )
          (hashtbl_find_list content c)
      )
      (
        List.filter (fun (p, _) -> (* 16882 *) p.[String.length p - 1] = '#') (hashtbl_find_list DeadMod.content m)
        |> List.map (fun (path, _) -> (* 8 *) String.sub path 0 (String.length path - 1))
      )
  in
  Hashtbl.iter merge_eq DeadMod.equal;
  Hashtbl.iter merge_eq DeadMod.equal


let report () =
  (* 1 *) prepare_report ();

  let rec report nb_call =
    (* 2 *) let l =
      let folder = fun acc (fn, path, loc) ->
        (* 40312 *) let rec cut_main s pos =
          (* 93 *) if pos = String.length s then (* 0 *) s
          else (* 93 *) if s.[pos] = '.' then (* 8 *) String.sub s (pos + 1) (String.length s - pos - 1)
          else (* 85 *) cut_main s (pos + 1)
        in
        if String.contains path '#' then (* 132 *) match hashtbl_find_list references path with
          | exception Not_found when (* 0 *) nb_call = 0 ->
                (* 0 *) (fn, cut_main path 0, loc, []) :: acc
          | exception Not_found -> (* 0 *) acc
          | l when (* 132 *) check_length nb_call l -> (* 8 *) (fn, cut_main path 0, loc, l) :: acc
          | _ -> (* 124 *) acc
        else (* 40180 *) acc
      in
      List.fold_left folder [] !decs
      |> List.fast_sort (fun (fn1, path1, loc1, _) (fn2, path2, loc2, _) ->
          (* 10 *) compare (fn1, loc1, path1) (fn2, loc2, path2))
    in

    let change =
      let (fn, _, _, _) = try List.hd l with _ -> (* 0 *) ("_none_", "", !last_loc, []) in
      dir fn
    in
    let pretty_print = fun (fn, path, loc, call_sites) ->
      (* 8 *) if change fn then (* 3 *) print_newline ();
      prloc ~fn loc;
      print_string path;
      if call_sites <> [] && !DeadFlag.obj.call_sites then (* 5 *) print_string "    Call sites:";
      print_newline ();
      if !DeadFlag.obj.call_sites then (* 8 *) begin
        List.iter (pretty_print_call ()) call_sites;
        if nb_call <> 0 then (* 5 *) print_newline ()
      end
    in

    let continue nb_call = (* 4 *) nb_call < !DeadFlag.obj.threshold in
    let s = if nb_call = 0 then (* 1 *) "UNUSED CLASS FIELDS" else (* 1 *) "ALMOST UNUSED CLASS FIELDS" in
    DeadCommon.report s l continue nb_call pretty_print report

  in report 0



                (********   WRAPPING  ********)


let wrap f x =
  (* 207221 *) if !DeadFlag.obj.print then (* 207221 *) f x else (* 0 *) ()

let add_alias src dest =
  (* 3 *) wrap (add_alias src) dest

let eom () =
  (* 697 *) wrap eom ()

let collect_export path u stock ?obj ?cltyp loc =
  (* 18447 *) wrap (collect_export path u stock ?obj ?cltyp) loc

let collect_references ?meth ?path expr =
  (* 1677 *) wrap (collect_references ?meth ?path) expr

let tstr cl_dec =
  (* 23 *) wrap tstr cl_dec

let add_var name expr =
  (* 59700 *) wrap (add_var name) expr

let class_structure cl_struct =
  (* 24 *) wrap class_structure cl_struct

let class_field cl_field =
  (* 141 *) wrap class_field cl_field

let arg typ args =
  (* 126508 *) wrap (arg typ) args

let report () =
  (* 1 *) wrap report ()
