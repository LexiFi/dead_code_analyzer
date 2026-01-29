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

let fill_from_cmt_tbl uid_to_decl res_uid_to_loc =
  let open Typedtree in
  let loc_of_item_decl = function
    | Value {val_loc = loc; _}
    | Value_binding {vb_pat = {pat_loc = loc; _}; _} ->
        Some loc.loc_start
    | _ -> None
  in
  let add_uid_loc uid item_decl =
    let loc = loc_of_item_decl item_decl in
    Option.iter (UidTbl.replace res_uid_to_loc uid) loc
  in
  UidTbl.iter add_uid_loc uid_to_decl;
  res_uid_to_loc

let cmt_decl_dep_to_loc_dep cmt_decl_dep uid_to_loc =
  let convert_pair (_dep_kind, uid_def, uid_decl) =
    let ( let* ) x f = Option.bind x f in
    let loc_opt_of_uid uid =
      UidTbl.find_opt uid_to_loc uid
    in
    let* def_loc = loc_opt_of_uid uid_def in
    let* decl_loc = loc_opt_of_uid uid_decl in
    Some (def_loc, decl_loc)
  in
  List.filter_map convert_pair cmt_decl_dep

let init cmt_infos cmti_uid_to_decl =
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
    |> cmt_decl_dep_to_loc_dep cmt_infos.cmt_declaration_dependencies
    |> Result.ok
  | _ -> Result.error "No implementation found in cmt_infos"
