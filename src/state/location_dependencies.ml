type t = (Lexing.position * Lexing.position) list

let empty = []

module UidTbl = Shape.Uid.Tbl

type uid_to_decl = Typedtree.item_declaration UidTbl.t

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

let find_opt_external_uid_loc ~cm_paths = function
  | Shape.Uid.(Compilation_unit _ | Internal | Predef _) -> None
  | Item {comp_unit; from; _} as uid ->
      let ( let* ) x f = Option.bind x f in
      let cached =
        match from with
        | Unit_info.Intf -> Cmt.cached_cmti comp_unit
        | Unit_info.Impl -> Cmt.cached_cmt comp_unit
      in
      let read_from_path () =
        let* cm_path =
          Utils.StringSet.elements cm_paths
          |> List.rev
          |> List.find_opt (fun path -> Utils.Filepath.unit path = comp_unit)
        in
        Cmt.read cm_path |> Result.to_option
      in
      let* cmi_cmt_infos =
        match cached with
        | Some _ as some -> some
        | None  -> read_from_path ()
      in
      let cmt_infos = snd cmi_cmt_infos in
      let cmt_uid_to_decl = cmt_infos.cmt_uid_to_decl in
      let* item_decl = UidTbl.find_opt cmt_uid_to_decl uid in
      loc_opt_of_item_decl item_decl

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
  res

let init ~cm_paths cmt_infos cmti_uid_to_decl =
  match cmt_infos.Cmt_format.cmt_annots with
  | Implementation _ ->
    let fill_from_cmti_tbl tbl =
      match cmti_uid_to_decl with
      | None -> tbl
      | Some cmti_uid_to_decl ->
          fill_from_cmt_tbl cmti_uid_to_decl tbl
    in
    (* TODO: Evaluate a generally good size for the tbl ? *)
    UidTbl.create 512
    |> fill_from_cmt_tbl cmt_infos.cmt_uid_to_decl
    |> fill_from_cmti_tbl
    |> cmt_decl_dep_to_loc_dep ~cm_paths cmt_infos.cmt_declaration_dependencies
    |> Result.ok
  | _ -> Result.error "No implementation found in cmt_infos"
