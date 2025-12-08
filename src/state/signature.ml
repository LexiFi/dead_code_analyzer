module UidTbl = Shape.Uid.Tbl

type t = {
  uid_to_loc : Lexing.position Shape.Uid.Tbl.t;
}

let empty () =
  let uid_to_loc =
    (* TODO: 512 is completely arbitrary. Evaluate a generally good size ? *)
    UidTbl.create 512
  in
  {uid_to_loc}

let fill_from_structure t (structure : Typedtree.structure) =
  let open Types in
  let rec fill_from_signature_item = function
    | Sig_value (_, {val_loc; val_uid; _}, _) ->
        UidTbl.replace t.uid_to_loc val_uid val_loc.loc_start
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
  t

let fill_from_cmt_infos t cmt_infos =
  let open Typedtree in
  let loc_of_item_decl = function
    | Value {val_loc = loc; _}
    | Value_binding {vb_pat = {pat_loc = loc; _}; _} ->
        Some loc.loc_start
    | _ -> None
  in
  let add_uid_loc uid item_decl =
    let loc = loc_of_item_decl item_decl in
    Option.iter (UidTbl.replace t.uid_to_loc uid) loc
  in
  Uid.Tbl.iter add_uid_loc cmt_infos.Cmt_format.cmt_uid_to_decl;
  t

let init file_infos =
  match file_infos.File_infos.cmt_infos with
  | None -> Result.error "No cmt_infos available"
  | Some ({cmt_annots = Implementation structure; _} as cmt_infos) -> (
      let res = fill_from_structure (empty ()) structure in
      let res = fill_from_cmt_infos res cmt_infos in
      match file_infos.cmti_infos with
      | None -> Result.ok res
      | Some cmti_infos ->
          fill_from_cmt_infos res cmti_infos
          |> Result.ok
  )
  | Some _ -> Result.error "No implementation found in cmt_infos"
