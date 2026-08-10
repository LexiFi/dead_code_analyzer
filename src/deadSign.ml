
(* Export helpers *)

let export_object ~path ~comp_unit ~stock id value =
  (* export a value as an object *)
  let path = Ident.create_persistent (Ident.name id ^ "*") :: path in
  let obj = value.Types.val_type in
  let loc = value.Types.val_loc in
  DeadObj.collect_export path comp_unit stock ~obj loc

let export_value ~path ~comp_unit ~stock id value =
  export_object ~path ~comp_unit ~stock id value;
  !DeadLexiFi.sig_value value;
  let state = State.get_current () in
  if Config.must_report_section state.config.sections.exported_values then
    let loc = value.Types.val_loc in
    DeadCommon.export path comp_unit stock id loc

let export_type ~path ~comp_unit ~stock id t =
  let path = id :: path in
  DeadType.collect_export path comp_unit stock t

let export_class ~path ~comp_unit ~stock id cd =
  let path = id :: path in
  let cltyp = cd.Types.cty_type in
  let loc = cd.Types.cty_loc in
  DeadObj.collect_export path comp_unit stock ~cltyp loc


let modtype ~on_mismatch (mt : Typedtree.module_type) =
  (* TODO: be more precise ?
      We probably only need to correct exports when [module type of] is
      used, and, for opt args only, when there is a substitution *)
  let types_sig = Utils.signature_of_modtype mt.mty_type in
  let typedtree_sig = Utils.typedtree_signature_of_modtype mt in
  match types_sig, typedtree_sig with
  | _::_, None -> on_mismatch types_sig
  | _ -> ()


(* Export functions *)

let rec correct_export : Types.signature_item -> unit = function
  | Sig_value (_, {Types.val_loc; _}, _)
    when not val_loc.Location.loc_ghost ->
      DeadCommon.unexport DeadCommon.decs val_loc;
      DeadObj.correct_export val_loc;
      (* For optional arguments, every use is stored during the analysis.
         The uses are then filtered before reporting. Thus, we need to
         remember "wrong" exports until then.
      *)
      let state = State.get_current () in
      if Config.must_report_opt_args state.config then
        Hashtbl.replace DeadCommon.implicit_decs val_loc.Location.loc_start ()

  | Sig_type (_, t, _, _) -> DeadType.correct_export t

  | Sig_class (_, {cty_loc; _}, _, _) -> DeadObj.correct_export cty_loc

  | Sig_module (_, _, {Types.md_type = t; _}, _, _)
  | Sig_modtype (_, {Types.mtd_type = Some t; _}, _) ->
      Utils.signature_of_modtype t
      |> List.iter correct_export
  | _ -> ()


let collect_export_from_signature ~path ~comp_unit signature =
  let state = State.get_current () in
  let mark_modtype_elements mt =
    (* For optional arguments, every use is stored during the analysis.
       The uses are then filtered before reporting. Thus, we need to
       remember "wrong" exports until then.
    *)
    let on_mismatch signature = List.iter correct_export signature in
    if Config.must_report_opt_args state.config then
      modtype ~on_mismatch mt
  in
  let rec collect_signature path Typedtree.{sig_items; _} =
    List.iter (collect_item path) sig_items
  and collect_item path sig_item =
    match sig_item.Typedtree.sig_desc with

    | Tsig_value {val_id; val_loc; val_val; _}
      when not val_loc.Location.loc_ghost ->
        let stock = DeadCommon.decs in
        export_value ~path ~comp_unit ~stock val_id val_val

    | Tsig_type (_, type_decls)->
        let stock = DeadCommon.decs in
        List.iter
          (fun Typedtree.{typ_id; typ_type; _} ->
            export_type ~path ~comp_unit ~stock typ_id typ_type
          )
          type_decls

    | Tsig_class class_descs ->
        let stock = DeadCommon.decs in
        List.iter
          (fun {Typedtree.ci_id_class; ci_decl; _} ->
            export_class ~path ~comp_unit ~stock ci_id_class ci_decl
          )
          class_descs

    | Tsig_module {md_id = Some id; md_type; _} ->
        let path = id :: path in
        Utils.typedtree_signature_of_modtype md_type
        |> Option.iter (collect_signature path);
        mark_modtype_elements md_type

    | Tsig_include {incl_mod; _} ->
        mark_modtype_elements incl_mod

    | _ -> ()
  in
  collect_signature path signature


let collect_export_from_structure ~path ~comp_unit structure =
  let met = Hashtbl.create 64 in
  let export export_fn ~path id param =
    let path_key =
      id :: path |> List.map Ident.name
    in
    match Hashtbl.find_opt met path_key with
    | Some _ -> () (* the current path is shadowed *)
    | None ->
        Hashtbl.add met path_key (); (* shadows other occurences of path *)
        let stock = DeadCommon.decs in
        export_fn ~path ~comp_unit ~stock id param
  in
  (* Traversal *)
  let open Typedtree in
  let rec collect_structure ~path Typedtree.{str_items; _} =
    (* We process the items in reverse order in order to keep only the
       last declaration of each path.
       E.g. in [struct let x = ... let x = ... end], the [x]s would
        appear in the order of their definitions in [str_items].
        The first one is shadowed by the second one, and  only the last
        one is actually exported.
    *)
    List.rev str_items
    |> List.iter (collect_item ~path)

  and collect_value ~path : value general_pattern -> unit =
    (* value bindings are of the form [pattern = exp] *)
    let value_of pat val_loc val_uid =
      let open Types in
      let val_type = pat.pat_type in
      let val_kind = Val_reg in
      let val_attributes = pat.pat_attributes in
      {val_type; val_kind; val_loc; val_attributes; val_uid}
    in
    fun pat -> match pat.pat_desc with
    | Tpat_any
    | Tpat_constant _
    | Tpat_variant (_, None, _) ->
        ()
    | Tpat_var (id, {loc; _}, uid) ->
        let value = value_of pat loc uid in
        export export_value ~path id value
    | Tpat_alias (sub_pat, id, {loc; _}, uid) ->
        let value = value_of pat loc uid in
        export export_value ~path id value;
        collect_value ~path sub_pat
    | Tpat_or (pat, _, _)
        (* In [P1 | P2], both branches must define the same names.
           Only the locations of the names in [P1] are used to refer to
           the corresponding values *)
    | Tpat_variant (_, Some pat, _)
    | Tpat_lazy pat ->
        collect_value ~path pat
    | Tpat_tuple pats
    | Tpat_construct (_, _, pats, _)
    | Tpat_array pats ->
        List.iter (collect_value ~path) pats
    | Tpat_record (fields, _) ->
        List.iter (fun (_, _, pat) -> collect_value ~path pat) fields

  and collect_module ~path m =
    match m.mod_desc with
    | Tmod_ident _
    | Tmod_unpack _ ->
        ()
    | Tmod_structure structure ->
        collect_structure ~path structure
    | Tmod_functor (_, m)
    | Tmod_apply (m, _, _)
    | Tmod_apply_unit m
    | Tmod_constraint (m, _, Tmodtype_implicit, _) ->
        collect_module ~path m
    | Tmod_constraint (_, _, Tmodtype_explicit mt, _) ->
        Utils.typedtree_signature_of_modtype mt
        |> Option.iter (collect_export_from_signature ~path ~comp_unit)

  and collect_module_binding ~path = function
    | {mb_id = Some id; mb_expr; _} ->
        let path = id::path in
        collect_module ~path mb_expr
    | _ -> ()

  and collect_class ~path c =
    let rec collect_class_expr ~path ce =
      match ce.cl_desc with
      | Tcl_ident _ ->
          ()
      | Tcl_structure _ ->
          export export_class ~path c.ci_id_class c.ci_decl
      | Tcl_fun (_, _, _, ce, _)
      | Tcl_apply (ce, _)
      | Tcl_let (_, _, _, ce)
      | Tcl_constraint (ce, _, _, _, _)
      | Tcl_open (_, ce) ->
          collect_class_expr ~path ce
    in
    collect_class_expr ~path c.ci_expr

  and collect_item ~path str_item =
    match str_item.str_desc with
    | Tstr_value (_, bindings) ->
        List.iter (fun {vb_pat; _} -> collect_value ~path vb_pat) bindings
    | Tstr_primitive {val_id; val_val; _} ->
        export export_value ~path val_id val_val
    | Tstr_type (_, type_decls)->
        List.iter
          (fun {typ_id; typ_type; _} ->
            export export_type ~path typ_id typ_type
          )
          type_decls
    | Tstr_module module_binding ->
        collect_module_binding ~path module_binding
    | Tstr_recmodule bindings ->
        List.iter (collect_module_binding ~path) bindings
    | Tstr_class class_decls ->
        List.iter (fun (class_decl, _) -> collect_class ~path class_decl) class_decls
    | Tstr_include {incl_mod; _} ->
        collect_module ~path incl_mod
    | _ -> ()
  in
  collect_structure ~path structure



let rec collect_export_from_include ~path sig_item =
  let comp_unit = DeadCommon._include in
  let stock = DeadCommon.incl in
  match (sig_item : Types.signature_item) with
  | Sig_value (id, ({val_loc; _} as value), _)
    when not val_loc.Location.loc_ghost ->
      export_object ~path ~comp_unit ~stock id value
  | Sig_class (id, cd, _, _) ->
      export_class ~path ~comp_unit ~stock id cd
  | Sig_module (id, _, {Types.md_type; _}, _, _) ->
      let path = id :: path in
      Utils.signature_of_modtype md_type
      |> List.iter (collect_export_from_include ~path)
  | _ -> ()


let correct_export sig_item =
  let state = State.get_current () in
  match state.file_infos.signature with
  | Some (Signature _) ->
      (* Typedtree signatures found in .cmti files do not need correction *)
      ()
  | _ -> correct_export sig_item
