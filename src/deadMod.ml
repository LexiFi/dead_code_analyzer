(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2025 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

open Types
open Typedtree


let map_item map_mod_type sig_item =
  let get_methods path loc obj_type =
    let res = ref [] in
    let add_meth meth_name =
      res := `Method (path, meth_name, loc) :: !res
    in
    DeadObj.iter_fields add_meth obj_type;
    !res
  in
  match sig_item with
  | Sig_value (id, val_desc, _) ->
    let val_name = Ident.name id in
    let val_loc = val_desc.val_loc.loc_start in
    let methods = get_methods val_name val_loc val_desc.val_type in
    (`Value (val_name, val_loc))::methods
  | Sig_type (id, {type_kind; _}, _, _) ->
      let t = Ident.name id in
      let build_cf cf_id cf_loc =
        let cf_name = Ident.name cf_id in
        let cf_loc = cf_loc.Location.loc_start in
        `Ctor_field (t ^ "." ^ cf_name, cf_loc)
      in
      let ids_locs =
        match type_kind with
        | Type_record (l, _) ->
            List.map (fun {Types.ld_id; ld_loc; _} -> (ld_id, ld_loc)) l
        | Type_variant (l, _) ->
            List.map (fun {Types.cd_id; cd_loc; _} -> (cd_id, cd_loc)) l
        | _ -> []
      in
      List.map (fun (id, loc) -> build_cf id loc) ids_locs
  | Sig_module (id, _, {md_type; _}, _, _)
  | Sig_modtype (id, {mtd_type = Some md_type; _}, _) ->
      let mod_name = Ident.name id in
      let update_path path = String.concat "." [mod_name; path] in
      List.map
        (fun elt ->
          match elt with
          | `Value (path, loc) -> `Value (update_path path, loc)
          | `Ctor_field (path, loc) -> `Ctor_field (update_path path, loc)
          | `Method (path, m, loc) -> `Method (update_path path, m, loc)
        )
       (map_mod_type md_type)
  | Sig_class (id, class_decl, _, _) ->
      let class_loc = class_decl.cty_loc.loc_start in
      let class_name = Ident.name id in
      let class_type =
        let rec sig_self = function
          | Cty_signature sg -> sg.csig_self
          | Cty_arrow (_, _, t)
          | Cty_constr (_, _, t)-> sig_self t
        in
        sig_self class_decl.cty_type
      in
      get_methods class_name class_loc class_type
  | _ -> []

let rec get_content ?select_param typ =
  List.map (map_item get_content) (Utils.signature_of_modtype ?select_param typ)
  |> List.flatten


let retrieve_signature mod_env mod_type =
  match mod_type with
  | Mty_ident _ | Mty_alias _ ->
      begin try
        let env = Utils.Envaux.load_env mod_env in
        Env.scrape_alias env mod_type
      with Envaux.(Error (Module_not_found _)) ->
        mod_type
      end
  | _ -> mod_type


let apply_by_content_requirement mod_expr expected_content actual_content =
  let elt_is_expected elt =
    expected_content = []
    || List.exists
      (fun exp ->
        match exp, elt with
        | `Value (path_exp, _), `Value (path_elt, _)
        | `Ctor_field (path_exp, _), `Ctor_field (path_elt, _) ->
            String.equal path_exp path_elt
        | `Method (path_exp, meth_exp, _), `Method (path_elt, meth_elt, _) ->
            String.equal path_exp path_elt && String.equal meth_exp meth_elt
        | _ -> false
      )
      expected_content
  in
  let state = State.get_current () in
  let state =
    let get_kind_and_loc = function
      | `Value (_, loc) -> `Value, loc
      | `Ctor_field (_, loc) -> `Ctor_field, loc
      | `Method (_, meth_name, loc) -> `Method meth_name, loc
    in
    List.fold_left
      (fun state elt ->
        let use_loc = mod_expr.mod_loc.Location.loc_start in
        if elt_is_expected elt then
          let elt_kind, elt_loc = get_kind_and_loc elt in
          State.add_use ~elt_kind ~elt_loc ~use_loc state
        else state
      )
      state
      actual_content
  in
  State.update state


let apply_by_requirement mod_expr =
  match mod_expr.mod_desc with
  | Tmod_constraint (me, mt, _, _) ->
    let exp_mt = retrieve_signature mod_expr.mod_env mt in
    let act_mt = retrieve_signature mod_expr.mod_env me.mod_type in
    let exp_content = get_content exp_mt in
    let act_content = get_content act_mt in
    apply_by_content_requirement mod_expr exp_content act_content
  | _ -> ()


let expr mod_expr =
  match mod_expr.mod_desc with
  | Tmod_apply (m1, m2, _) ->
      (* Add a use for each element in [m2] expected by [m1] *)
      let exp_elts = get_content ~select_param:true m1.mod_type in
      let arg_elts = get_content m2.mod_type in
      apply_by_content_requirement mod_expr exp_elts arg_elts
  | _ -> ()


let type_ mt =
  let on_mismatch signature =
    List.iter DeadSign.correct_export signature
  in
  DeadSign.modtype ~on_mismatch mt


                (********   WRAPPING  ********)

let expr m =
  let state = State.get_current () in
  if [@warning "-44"] Config.must_report_main state.config then
    expr m
  else ()
