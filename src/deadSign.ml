type context =
  | Toplevel
  | In_module of (Ident.t * Location.t)
  | In_modtyp of (Ident.t * Location.t)
  | Include

let should_export_value ~context ~stock loc =
  let state = State.get_current () in
  let belongs_to_context loc =
    match context with
    | Toplevel | Include -> true
    | In_module (_, md_loc)
    | In_modtyp (_, md_loc) ->
        (* When a value is part of a module sig because of:
           - an include, then its location precedes that of the current module;
           - a module type with substitution, then its location ends
             with the current module's sig.
           Checking that the value's location is striclty within the
           module's rules out these 2 cases.
          *)
        let get_pos_info loc =
          let fname, start_l, start_c =
            Location.get_pos_info loc.Location.loc_start
          in
          let _, end_l, end_c = Location.get_pos_info loc.loc_end in
          fname, (start_l, start_c), (end_l, end_c)
        in
        let v_fname, v_start, v_end = get_pos_info loc in
        let md_fname, md_start, md_end = get_pos_info md_loc in
        let ( > ) (l1, c1) (l2, c2) =
          l1 > l2 || (l1 = l2 && c1 > c2)
        in
        String.equal v_fname md_fname
        && v_start > md_start
        && md_end > v_end
  in
  Config.must_report_section state.config.sections.exported_values
  && (* do not add the loc in decs if it belongs to a module type
        or if it is not actually declared in the current context *)
    ( stock != DeadCommon.decs
      || (not (Hashtbl.mem DeadCommon.in_modtype loc.Location.loc_start)
          && belongs_to_context loc)
    )

let collect_export ~context ~path ~comp_unit ~stock sig_item =
  let rec collect_export context path stock : Types.signature_item -> unit =
    function

    | Sig_value (id, ({Types.val_loc; val_type; _} as value), _)
      when not val_loc.Location.loc_ghost ->
        if should_export_value ~context ~stock val_loc then
          DeadCommon.export path comp_unit stock id val_loc;
        let path = Ident.create_persistent (Ident.name id ^ "*") :: path in
        DeadObj.collect_export path comp_unit stock ~obj:val_type val_loc;
        !DeadLexiFi.sig_value value

    | Sig_type (id, t, _, _) when stock == DeadCommon.decs ->
        DeadType.collect_export (id :: path) comp_unit stock t

    | Sig_class (id, {Types.cty_type = t; cty_loc = loc; _}, _, _) ->
        DeadObj.collect_export (id :: path) comp_unit stock ~cltyp:t loc

    | (Sig_module (id, _, {Types.md_type = t; md_loc = loc; _}, _, _)
    | Sig_modtype (id, {Types.mtd_type = Some t; mtd_loc = loc; _}, _)) as s ->
        let stock, context =
          match s, context with
          | _, Include -> stock, Include
          | Sig_modtype _, _ -> DeadCommon.in_modtype, In_modtyp (id, loc)
          | _, _ -> stock, In_module (id, loc)
        in
        Utils.signature_of_modtype t
        |> List.iter (collect_export context (id::path) stock)

    | _ -> ()
  in
  collect_export context path stock sig_item

let collect_export_from_typedtree ~path ~comp_unit signature =
  let should_export_value =
    let state = State.get_current () in
    Config.must_report_section state.config.sections.exported_values
  in
  let rec collect_signature path Typedtree.{sig_items; _} =
    List.iter (collect_item path) sig_items
  and collect_item path sig_item =
    match sig_item.Typedtree.sig_desc with

    | Tsig_value {val_id; val_loc; val_val; _}
      when not val_loc.Location.loc_ghost ->
        if should_export_value then
          DeadCommon.export path comp_unit DeadCommon.decs val_id val_loc;
        let path = Ident.create_persistent (Ident.name val_id ^ "*") :: path in
        let obj = val_val.val_type in
        DeadObj.collect_export path comp_unit DeadCommon.decs ~obj val_loc;
        !DeadLexiFi.sig_value val_val

    | Tsig_type (_, type_decls)->
        let export_type (td : Typedtree.type_declaration) =
          let path = td.typ_id :: path in
          DeadType.collect_export path comp_unit DeadCommon.decs td.typ_type
        in
        List.iter export_type type_decls

    | Tsig_class class_descs ->
        let export_class (cd : Typedtree.class_description) =
          let path = cd.ci_id_class :: path in
          let cltyp = cd.ci_expr.cltyp_type in
          DeadObj.collect_export path comp_unit DeadCommon.decs ~cltyp cd.ci_loc
        in
        List.iter export_class class_descs

    | Tsig_module {md_id = Some id; md_type; _} ->
        let path = id :: path in
        Utils.typedtree_signature_of_modtype md_type
        |> Option.iter (collect_signature path)

    | _ -> ()
  in
  collect_signature path signature
