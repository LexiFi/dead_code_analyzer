type context =
  | Toplevel
  | In_module of (Ident.t * Location.t)
  | Include


let export_value should_export ~path ~comp_unit ~stock id value =
  let loc = value.Types.val_loc in
  if should_export then
    DeadCommon.export path comp_unit stock id loc;
  let path = Ident.create_persistent (Ident.name id ^ "*") :: path in
  let obj = value.Types.val_type in
  DeadObj.collect_export path comp_unit stock ~obj loc;
  !DeadLexiFi.sig_value value

let export_type ~path ~comp_unit ~stock id t =
  let path = id :: path in
  DeadType.collect_export path comp_unit stock t

let export_class ~path ~comp_unit ~stock id cd =
  let path = id :: path in
  let cltyp = cd.Types.cty_type in
  let loc = cd.Types.cty_loc in
  DeadObj.collect_export path comp_unit stock ~cltyp loc

let should_export_value ~context ~stock loc =
  let state = State.get_current () in
  let belongs_to_context loc =
    match context with
    | Toplevel | Include -> true
    | In_module (_, md_loc) ->
        (* When a value is part of a module sig because of an include,
           then its location precedes that of the current module.
        *)
        let get_pos_info loc =
            Location.get_pos_info loc.Location.loc_start
        in
        let v_fname, v_line, v_col = get_pos_info loc in
        let md_fname, md_line, md_col = get_pos_info md_loc in
        String.equal v_fname md_fname
        && (v_line, v_col) > (md_line, md_col)
  in
  Config.must_report_section state.config.sections.exported_values
  && (* do not add the loc in decs if it is not actually declared in
        the current context *)
    (stock != DeadCommon.decs || belongs_to_context loc)

let rec collect_export ~context ~path ~comp_unit ~stock sig_item =
  match (sig_item : Types.signature_item) with

  | Sig_value (id, ({val_loc; _} as value), _)
    when not val_loc.Location.loc_ghost ->
      let should_export = should_export_value ~context ~stock val_loc in
      export_value should_export ~path ~comp_unit ~stock id value

  | Sig_type (id, t, _, _) when stock == DeadCommon.decs ->
      export_type ~path ~comp_unit ~stock id t

  | Sig_class (id, cd, _, _) ->
      export_class ~path ~comp_unit ~stock id cd

  | Sig_module (id, _, {Types.md_type = t; md_loc = loc; _}, _, _) ->
      let context =
        match context with
        | Include -> context
        | _ -> In_module (id, loc)
      in
      let path = id :: path in
      Utils.signature_of_modtype t
      |> List.iter (collect_export ~context ~path ~comp_unit ~stock)

  | _ -> ()


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


let modtype ~on_mismatch (mt : Typedtree.module_type) =
  let types_sig = Utils.signature_of_modtype mt.mty_type in
  let typedtree_sig = Utils.typedtree_signature_of_modtype mt in
  match types_sig, typedtree_sig with
  | _::_, None -> on_mismatch types_sig
  | _ -> ()


let collect_export_from_typedtree ~path ~comp_unit signature =
  let state = State.get_current () in
  let should_export_value =
    Config.must_report_section state.config.sections.exported_values
  in
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
        export_value should_export_value ~path ~comp_unit ~stock val_id val_val

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


let correct_export sig_item =
  let state = State.get_current () in
  match state.file_infos.cm_sign with
  | Some (Cmti_sign _) ->
      (* Typedtree signatures found in .cmti files do not need correction *)
      ()
  | _ -> correct_export sig_item
