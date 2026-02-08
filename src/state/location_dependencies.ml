type t = (Lexing.position * Lexing.position) list

let empty = []

module UidTbl = Shape.Uid.Tbl

type uid_to_decl = Typedtree.item_declaration UidTbl.t

let fill_from_structure (structure : Typedtree.structure) res_uid_to_loc =
  let open Types in
  let rec fill_from_signature_item = function
    | Sig_value (_, {val_loc; val_uid; _}, _) ->
        UidTbl.replace res_uid_to_loc val_uid val_loc.loc_start
    | Sig_module (_, _, {md_type = modtype; _}, _, _)
    | Sig_modtype (_, {mtd_type = Some modtype; _}, _) ->
        Utils.signature_of_modtype modtype
        |> fill_from_signature
    | _ -> ()
  and fill_from_signature s =
    List.iter fill_from_signature_item s
  in
  let iterator =
    let super = Tast_iterator.default_iterator in
    let structure_item self struct_item =
      let open Typedtree in
      begin match struct_item.str_desc with
      | Tstr_include {incl_type; _} -> fill_from_signature incl_type
      | _ -> ()
      end;
      super.Tast_iterator.structure_item self struct_item
    in
    {super with structure_item}
  in
  iterator.structure iterator structure;
  res_uid_to_loc

let loc_opt_of_item_decl = function
  | Typedtree.Value {val_loc = loc; _}
  | Typedtree.Value_binding {vb_pat = {pat_loc = loc; _}; _} ->
      Some loc.loc_start
  | _ -> None

let fill_from_cmt_tbl uid_to_decl res_uid_to_loc =
  let add_uid_loc uid item_decl =
    let loc = loc_opt_of_item_decl item_decl in
    Option.iter (UidTbl.replace res_uid_to_loc uid) loc
  in
  UidTbl.iter add_uid_loc uid_to_decl;
  res_uid_to_loc

let find_opt_external_uid_loc, clear_external_cache =
  let cache = Hashtbl.create 16 in
  let clear_external_cache () = Hashtbl.reset cache in
  let find_opt_external_uid_loc ~cm_paths = function
    | Shape.Uid.(Compilation_unit _ | Internal | Predef _) -> None
    | Item {comp_unit; _} as uid ->
      let ( let* ) x f = Option.bind x f in
      let* cmt_uid_to_decl =
        match Hashtbl.find_opt cache comp_unit with
        | Some _ as cmt_uid_to_decl -> cmt_uid_to_decl
        | None ->
            let* cm_path =
                Utils.StringSet.elements cm_paths
                |> List.rev
                |> List.find_opt (fun path -> Utils.Filepath.unit path = comp_unit)
            in
            let* cmt_infos = Cmt_format.read cm_path |> snd in
            let cmt_uid_to_decl = cmt_infos.cmt_uid_to_decl in
            Hashtbl.add cache comp_unit cmt_uid_to_decl;
            Some cmt_uid_to_decl
      in
      let* item_decl = UidTbl.find_opt cmt_uid_to_decl uid in
      loc_opt_of_item_decl item_decl
  in
  find_opt_external_uid_loc, clear_external_cache

let cmt_decl_dep_to_loc_dep ~cm_paths cmt_decl_dep uid_to_loc =
  let convert_pair (_dep_kind, uid_def, uid_decl) =
    let ( let* ) x f = Option.bind x f in
    let loc_opt_of_uid uid =
      match UidTbl.find_opt uid_to_loc uid with
      | Some _ as loc -> loc
      | None -> find_opt_external_uid_loc ~cm_paths uid
    in
    let* def_loc = loc_opt_of_uid uid_def in
    let* decl_loc = loc_opt_of_uid uid_decl in
    Some (def_loc, decl_loc)
  in
  let res = List.filter_map convert_pair cmt_decl_dep in
  clear_external_cache ();
  res

let init ~cm_paths cmt_infos cmti_uid_to_decl =
  match cmt_infos.Cmt_format.cmt_annots with
  | Implementation structure ->
    let fill_from_cmti_tbl tbl =
      match cmti_uid_to_decl with
      | None -> tbl
      | Some cmti_uid_to_decl ->
          fill_from_cmt_tbl cmti_uid_to_decl tbl
    in
    (* TODO: Evaluate a generally good size for the tbl ? *)
    UidTbl.create 512
    |> fill_from_structure structure
    |> fill_from_cmt_tbl cmt_infos.cmt_uid_to_decl
    |> fill_from_cmti_tbl
    |> cmt_decl_dep_to_loc_dep ~cm_paths cmt_infos.cmt_declaration_dependencies
    |> Result.ok
  | _ -> Result.error "No implementation found in cmt_infos"
