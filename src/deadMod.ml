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

open DeadCommon



let item maker = function
  | Sig_value (id, {val_loc = {Location.loc_start= loc; _}; _}, _) ->
      (`Value (Ident.name id, loc))::[]
  | Sig_type (id, {type_kind; _}, _, _) ->
      let t = Ident.name id in
      begin match type_kind with
      | Type_record (l, _) ->
          List.map
            (fun {Types.ld_id; ld_loc = {Location.loc_start = loc; _}; _} ->
              `Type (t ^ "." ^ (Ident.name ld_id), loc)
            )
            l
      | Type_variant (l, _) ->
          List.map
            (fun {Types.cd_id; cd_loc = {Location.loc_start = loc; _}; _} ->
              `Type (t ^ "." ^ Ident.name cd_id, loc)
            )
            l
      | _ -> []
    end
  | Sig_module (id, _, {md_type; _}, _, _)
  | Sig_modtype (id, {mtd_type = Some md_type; _}, _) ->
      List.map
        (fun elt ->
          let update_path (path, loc) = (Ident.name id ^ "." ^ path, loc) in
          match elt with
          | `Value v -> `Value (update_path v)
          | `Type t -> `Type (update_path t)
          | `Method v -> `Method (update_path v)
        )
       (maker md_type)
  | Sig_class (id, {cty_loc = {Location.loc_start = loc; _}; _}, _, _) ->
      (* TODO: actually explore the methods in the class_declaration *)
    `Method (Ident.name id ^ "#", loc) :: []
  | _ -> []

let rec make_content typ =
  List.map (item make_content) (Utils.signature_of_modtype typ)
  |> List.flatten


let rec make_arg typ =
  List.map (item make_arg) (Utils.signature_of_modtype ~select_param:true typ)
  |> List.flatten


let expr m = match m.mod_desc with
  | Tmod_apply (m1, m2, _) ->
      (* Add a use for each element in [m2] expected by [m1] *)
      let exp_elts = make_arg m1.mod_type in
      let arg_elts = make_content m2.mod_type in
      let elt_is_expected elt =
        exp_elts = []
        || List.exists
          (fun exp ->
            match exp, elt with
            | `Value (path_exp, _), `Value (path_elt, _)
            | `Type (path_exp, _), `Type (path_elt, _)
            | `Method (path_exp, _), `Method (path_elt, _) ->
                String.equal path_exp path_elt
            | _ -> false
          )
          exp_elts
      in
      List.iter
        (fun elt ->
          let state = State.get_current () in
          let sections = state.config.sections in
          let use_loc = m.mod_loc.Location.loc_start in
          if elt_is_expected elt then
            match elt with
            | `Value (_, val_loc) when exported sections.exported_values val_loc ->
                State.Values.add_use ~val_loc ~use_loc state.values
                |> ignore
            | `Type (_, cf_loc) when exported ~is_type:true sections.types cf_loc ->
                State.Ctors_fields.add_use ~cf_loc ~use_loc state.ctors_fields
                |> ignore
            | `Method _ when Config.must_report_section sections.methods ->
              (* TODO *)
              ()
            | _ -> ()
        )
        arg_elts
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
