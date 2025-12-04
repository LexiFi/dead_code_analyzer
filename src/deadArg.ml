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

let at_eof = ref []
let at_eocb = ref []

let met = Hashtbl.create 512

let eof () =
  List.iter (fun f -> f ()) !at_eof;
  at_eof := [];
  Hashtbl.reset met

let eocb () =
  List.iter (fun f -> f ()) !at_eocb

let increment_count label count_tbl =
  let count = Hashtbl.find_opt count_tbl label |> Option.value ~default:0 in
  let count = count + 1 in
  Hashtbl.replace count_tbl label count;
  count

let register_use label expr builddir loc last_loc count_tbl =
  let has_val =
    match expr.exp_desc with
    | Texp_construct (_, {cstr_name = "None"; _}, _) -> false
    | _ -> true
  in
  let count = increment_count label count_tbl in
  let call_site =
    if expr.exp_loc.Location.loc_ghost then last_loc
    else expr.exp_loc.Location.loc_start
  in
  if check_underscore label then
    let loc = VdNode.find loc label count in
    if not (Hashtbl.mem met (last_loc, loc, label)) then (
      Hashtbl.add met (last_loc, loc, label) ();
      let opt_arg_use = {
          builddir;
          decl_loc = loc;
          label;
          has_val;
          use_loc = call_site;
       }
      in
      opt_args := opt_arg_use :: !opt_args
    )

let deferrable_register_use label expr builddir loc last_loc count_tbl =
  let register_use () = register_use label expr builddir loc last_loc count_tbl in
  if VdNode.is_end loc then
    let fn = loc.Lexing.pos_fname in
    if fn.[String.length fn - 1] = 'i' then
      (* TODO:
       * What does it mean to have a loc in a signature ?
       * When does it happen ? *)
      at_eocb := register_use :: !at_eocb
    else if !depth > 0 then at_eof := register_use :: !at_eof
    else register_use ()
  else register_use ()

let rec register_uses builddir loc args =
  List.iter
    (fun (_, e) -> Option.iter (register_higher_order_uses builddir) e)
    args;
  if is_ghost loc then () (* Ghostbuster *)
  else
    let count_tbl = Hashtbl.create 8 in
    let register_opt_arg_use = function
      | (Asttypes.Optional label, Some expr) ->
          deferrable_register_use label expr builddir loc !last_loc count_tbl
      | _ -> ()
    in
    List.iter register_opt_arg_use args

(* Verify the nature of the argument to detect and treat function applications and uses *)
and register_higher_order_uses builddir e =
  (* Optional arguments expected by arrow-typed parameter are considered used
   * because they are necessary to match the expected signature *)
  let register_opt_args_uses loc typ =
    let rec get_labels labels typ =
      match get_deep_desc typ with
      | Tarrow (arg_label, _, t, _) ->
          let labels =
            match arg_label with
            | Asttypes.Optional label -> label :: labels
            | _ -> labels
          in
          get_labels labels t
      | _ -> labels
    in
    let labels = get_labels [] typ in
    let count_tbl = Hashtbl.create 8 in
    let register_opt_arg_use label =
      deferrable_register_use label e builddir loc !last_loc count_tbl
    in
    List.iter register_opt_arg_use labels
  in
  match e.exp_desc with
  | Texp_ident (_, _, {val_loc = {Location.loc_start = loc; _}; _}) ->
      register_opt_args_uses loc e.exp_type
  | Texp_apply (exp, _) -> (
      match exp.exp_desc with
      | Texp_ident (_, _, {val_loc = {Location.loc_start = loc; loc_ghost; _}; _})
      | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; loc_ghost; _}; _}) ->
          register_opt_args_uses loc e.exp_type;
          (* TODO: Why do we want to set last_loc here ? *)
          if not loc_ghost then last_loc := loc
      | _ -> ()
    )
  | Texp_let (_, [binding], expr) -> (
      (* Partial application as argument may be cut in two parts:
       * [let _ = binding in expr] with [expr] discarding opt args *)
      let ( let$ ) x f = Option.iter f x in
      let$ ident_loc =
        match binding.vb_expr.exp_desc with
        | Texp_apply ({exp_desc = Texp_ident (_, _, val_desc); _}, _)
        | Texp_ident (_, _, val_desc) ->
            Some val_desc.val_loc.loc_start
        | _ -> None
      in
      let$ (c_lhs, c_rhs) =
        match expr.exp_desc with
        | Texp_function (_, Tfunction_cases {cases = [case]; _}) ->
            Some (case.c_lhs, case.c_rhs)
        | _ -> None
      in
      match (c_lhs.pat_desc, c_rhs.exp_desc) with
      | (Tpat_var _, Texp_apply (_, args)) ->
          if c_lhs.pat_loc.loc_ghost && c_rhs.exp_loc.loc_ghost
             && expr.exp_loc.loc_ghost
          then register_uses builddir ident_loc args
      | _ -> ()
    )
  | _ -> ()

(* redefine without the [builddir] parameter *)
let register_uses val_loc args =
  let builddir =
    let state = State.get_current () in
    State.File_infos.get_builddir state.file_infos
  in
  register_uses builddir val_loc args

let rec bind loc expr =
  match expr.exp_desc with
  | Texp_function (params, body) -> (
      let check_param_style = function
        | Tparam_pat {pat_type; _}
        | Tparam_optional_default ({pat_type; _}, _) ->
            DeadType.check_style pat_type expr.exp_loc.Location.loc_start
      in
      let register_optional_param = function
        | Asttypes.Optional s when DeadFlag.(!optn.print || !opta.print) ->
            let (opts, next) = VdNode.get loc in
            VdNode.update loc (s :: opts, next)
        | _ -> ()
      in
      List.iter
        (fun {fp_kind; fp_arg_label; _} ->
          check_param_style fp_kind;
          register_optional_param fp_arg_label
        )
        params;
      match body with
      | Tfunction_body exp -> bind loc exp
      | Tfunction_cases {cases = [{c_lhs = {pat_type; _}; c_rhs = exp; _}]; _} ->
          DeadType.check_style pat_type expr.exp_loc.Location.loc_start;
          bind loc exp
      | _ -> ()
    )
  | exp_desc
    when (!DeadFlag.optn.print || !DeadFlag.opta.print)
         && DeadType.nb_args ~keep:`Opt expr.exp_type > 0 ->
      let ( let$ ) x f = Option.iter f x in
      let$ loc2 =
        match exp_desc with
        | Texp_apply ({exp_desc = Texp_ident (_, _, {val_loc = loc; _}); _}, _)
        | Texp_apply ({exp_desc = Texp_field (_, _, {lbl_loc = loc; _}); _}, _)
        | Texp_ident (_, _, {val_loc = loc; _}) ->
            Some loc.loc_start
        | _ -> None
      in
      VdNode.merge_locs loc loc2
  | _ -> ()

                (********   WRAPPING  ********)

let wrap f x y =
  if DeadFlag.(!optn.print || !opta.print) then f x y else ()

let register_uses val_loc args = wrap register_uses val_loc args
