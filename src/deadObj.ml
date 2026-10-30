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



                (********   ATTRIBUTES  ********)

let at_eocb = ref []

let last_class = ref Lexing.dummy_pos            (* last class met *)


                (********   HELPERS   ********)


let add_path elt_path elt_loc state =
  State.add_loc_binding ~elt_kind:`Object ~elt_path ~elt_loc state

let add_path_alias orig_path alias_path state =
  State.add_local_path_alias ~elt_kind:`Object ~orig_path ~alias_path state


let get_loc obj_path =
  let state = State.get_current () in
  let obj_path = State.Methods.get_orig_path ~obj_path state.methods in
  State.find_loc ~elt_kind:`Object ~elt_path:obj_path state


let add_equal loc1 loc2 state =
  if is_ghost loc1 || is_ghost loc2 then state
  else begin
    if loc1 = !last_class then
      last_class := loc2;
    State.add_alias ~elt_kind:`Object ~orig_loc:loc2 ~alias_loc:loc1 state
  end



let rec iter_fields action typ = match get_deep_desc typ with
  | Tobject (t, _)
  | Tarrow (_, _, t, _) -> iter_fields action t
  | Tfield (s, k, _, t) ->
      if field_kind_repr k <> Fabsent && s.[0] > 'Z' then
        action s;
      iter_fields action t
  | _ -> ()


let rec repr_exp expr f =
  match expr.exp_desc with
    | Texp_function _ as exp_desc ->
        begin match Utils.Compat.get_function_bodies exp_desc with
        | Ok (expr::_) -> repr_exp expr f
        | _ -> assert false
        end
    | Texp_sequence (_, expr)
    | Texp_let (_, _, expr)
    | Texp_apply (expr, _) -> repr_exp expr f
    | _ -> f expr

let locate expr =
  let locate expr =
    match expr.exp_desc with
    | Texp_instvar (_, _, {Asttypes.loc; _})
    | Texp_new (_, _, {Types.cty_loc=loc; _})
    | Texp_ident (_, _, {Types.val_loc=loc; _}) ->
        loc.Location.loc_start
    | _ -> Lexing.dummy_pos
  in repr_exp expr locate


let eof () =
  last_class := Lexing.dummy_pos



                (********   PROCESSING  ********)


let collect_from_include ?incl_path ~rev_curr_path path =
  let alias_path = List.rev path |> String.concat "." in
  let rev_mod_path =
    match incl_path with
    | None -> rev_curr_path
    | Some incl_path -> Utils.normalize_mod_path ~rev_curr_path incl_path
  in
  let orig_path = List.rev (alias_path::rev_mod_path) |> String.concat "." in
  State.get_current ()
  |> add_path_alias orig_path alias_path
  |> State.update


let collect_export path ~obj ~cltyp loc =
  let state = State.get_current () in
  let pos = loc.Location.loc_start in
  let state =
    match List.rev path with
    | h :: t ->
        let short = String.concat "." t in
        let path = h ^ "." ^ short in
        add_path_alias path short state
        |> add_path path pos
    | _ -> state
  in
  State.update state;

  let save id =
    let state = State.get_current () in
    let sourcepath = State.File_infos.get_sourcepath state.State.file_infos in
    (* TODO: resolve the builddir ('/workspace_root') in the case of dune
       compiled projects. Without it, looking up for an existing csml below will
       always fail and can lead to false positive *)
    if not (Sys.file_exists (Filename.remove_extension sourcepath ^ ".csml")) then begin
      let elt_path =
        String.concat "." (List.rev path)
        ^ "#" ^ id
      in
      let elt_loc = loc.Location.loc_start in
      let elt_kind = `Method id in
      State.add_exported_declaration ~elt_kind ~elt_loc ~elt_path state
      |> State.update
    end
  in
  let save id =
    if DeadCommon.should_export loc id then save id
  in


  let rec sig_self = function
    | Cty_signature sg -> Some (sg.csig_self)
    | Cty_arrow (_, _, t) -> sig_self t
    | Cty_constr _ -> None (* do not track class types' methods *)
  in
  let typ = match cltyp with
    | None -> obj
    | Some cltyp -> sig_self cltyp
  in
  match typ with
    | Some typ ->
        iter_fields save typ
    | None -> ()

let correct_export loc =
  let state = State.get_current () in
  let elt_loc = loc.Location.loc_start in
  State.remove_exported_declaration ~elt_kind:`Object ~elt_loc state
  |> State.update


let collect_references ~meth ~call_site expr =
  let obj_loc = locate expr in

  if not (is_ghost obj_loc) then begin
    let state = State.get_current () in
    let meth_name = meth in
    let use_loc = call_site in
    let elt_kind = `Method meth_name in
    let state =
      State.add_use ~elt_kind ~elt_loc:obj_loc ~use_loc state
    in
    let state =
      let obj_loc = State.Methods.get_orig_loc ~obj_loc state.methods in
      if obj_loc = !last_class then
        State.add_self_use ~elt_kind ~elt_loc:obj_loc ~use_loc state
      else
        state
    in
    State.update state
  end


let tstr ({ci_expr; ci_decl = {cty_loc = loc; _}; ci_id_name = {txt = name; _}; _}, _) =
  let state = State.get_current () in
  let loc = loc.Location.loc_start in
  last_class := loc;
  let short =
    (List.rev !mods |> String.concat ".")
    ^ (if !mods <> [] then "." else "") ^ name
  in
  let modname = State.File_infos.get_modname state.file_infos in
  let path = modname ^ "." ^ short in
  let state =
    match State.Methods.find_orig_path ~obj_path:short state.methods with
    | None -> add_path_alias path short state
    | Some _ ->
        let loc =
          match get_loc short with
          | None -> get_loc path
          | some -> some
        in
        match loc with
        | Some loc when loc <> !last_class -> add_equal !last_class loc state
        | _ -> add_path path !last_class state
  in

  let rec make_dep ci_expr state =
    match ci_expr.cl_desc with
    | Tcl_ident (path, _, _) ->
        begin match get_loc (Path.name path) with
        | None -> state
        | Some orig_loc -> add_equal !last_class orig_loc state
        end
    | Tcl_fun (_, _, _, ci_expr, _)
    | Tcl_constraint (ci_expr, _, _, _, _) -> make_dep ci_expr state
    | _ -> state
  in
  make_dep ci_expr state
  |> State.update


let add_var loc expr =
  let rec kind expr =
    let find_first_kind exprs =
      (* For alternative results: find the first non-`Ignore kind *)
      List.find_map
        (fun expr ->
          match repr_exp expr kind with
          | `Ignore -> None
          | res -> Some res
        )
        exprs
      |> Option.value ~default:`Ignore
    in
    let find_first_case_kind cases =
      List.map (fun {c_rhs; _} -> c_rhs) cases
      |> find_first_kind
    in
    match expr.exp_desc with
    (* Result identified *)
    | Texp_object _ ->
        `Obj
    | Texp_new (_, _, {cty_loc = {Location.loc_start = cty_loc; _}; _}) ->
        `New cty_loc
    | Texp_ident (_, _, {Types.val_loc; _}) ->
        `Ident val_loc.Location.loc_start
    (* Cases not traversed by repr_exp *)
    | Texp_match _ as exp_desc ->
        let (_, cases, _, _) = Utils.Compat.get_match_data_exn exp_desc in
        find_first_case_kind cases
    | Texp_try _ as exp_desc ->
        let (_, cases, _) = Utils.Compat.get_try_data_exn exp_desc in
        find_first_case_kind cases
    | Texp_ifthenelse (_, then_, Some else_) ->
        find_first_kind [then_; else_]
    (* Default *)
    | _ -> `Ignore
  in
  let state = State.get_current () in
  let state =
    match repr_exp expr kind with
    | `Obj ->
        last_class := loc;
        state
    | `New cty_loc -> add_equal loc cty_loc state
    | `Ident id_loc ->
        let expr_loc : Location.t = expr.exp_loc in
        if id_loc >= expr_loc.loc_start && id_loc <= expr_loc.loc_end then
          (* ident is defined within expr *)
          add_equal id_loc loc state
        else state
    | `Ignore -> state
  in
  State.update state


let class_structure cl_struct =
  let rec add_aliases pat state =
    let state =
      match pat.pat_desc with
      | Tpat_alias _
      | Tpat_var _ when not pat.pat_loc.Location.loc_ghost ->
          add_equal pat.pat_loc.Location.loc_start !last_class state
      | _ -> state
    in
    match Utils.Compat.get_alias_data pat.pat_desc with
    | Ok (pat, _, _, _) -> add_aliases pat state
    | _ -> state
  in
  State.get_current ()
  |> add_aliases cl_struct.cstr_self
  |> State.update

let collect_inheritance ~inherited_path ~elt_loc ~methods state =
  let state = (* mark each method as inherited *)
    List.fold_left
      (fun state (meth_name, _) ->
        let elt_kind = `Method meth_name in
        State.mark_inherited ~elt_kind ~elt_loc ~inherited_path state
      )
      state
      methods
  in
  State.inherit_initializer ~elt_kind:`Object ~elt_loc ~inherited_path state

let add_equal_parent ~inherited_path ~cl_exp ~cf state =
  let cl_exp_loc = cl_exp.cl_loc.Location.loc_start in
  let state =
    let cf_loc = cf.cf_loc.Location.loc_start in
    add_equal cf_loc cl_exp_loc state
  in
  match get_loc inherited_path with
  | None ->
      (* path has not been located yet. Try again later. *)
      let equal () =
        let state = State.get_current () in
        let state =
          match get_loc inherited_path with
          | None -> state
          | Some orig_loc -> add_equal cl_exp_loc orig_loc state
        in
        State.update state
      in
      at_eocb := equal :: !at_eocb;
      state
  | Some loc ->
      add_equal cl_exp_loc loc state

let class_field cf =
  let rec locate cl_exp (state: State.t) =
    match cl_exp.cl_desc with
    | Tcl_ident (path, _, _) ->
        let path = Path.name path in
        State.Methods.get_orig_path ~obj_path:path state.methods
    | Tcl_fun (_, _, _, cl_exp, _)
    | Tcl_apply (cl_exp, _)
    | Tcl_let (_, _, _, cl_exp)
    | Tcl_constraint (cl_exp, _, _, _, _) -> locate cl_exp state
    | Tcl_structure _ | Tcl_open _ -> _none
  in
  let state = State.get_current () in
  let state =
    let elt_loc = !last_class in
    match cf.cf_desc with
    | Tcf_inherit (_, cl_exp, _, _, methods) ->
        let inherited_path = locate cl_exp state in
        if inherited_path == _none then state
        else
          collect_inheritance ~inherited_path ~elt_loc ~methods state
          |> add_equal_parent ~inherited_path ~cl_exp ~cf

    | Tcf_method ({txt; _}, _, Tcfk_virtual _) ->
        let elt_kind = `Method txt in
        State.mark_virtual ~elt_kind ~elt_loc state
    | Tcf_method ({txt; _}, _, _) ->
        let elt_kind = `Method txt in
        State.mark_defined ~elt_kind ~elt_loc state
    | Tcf_initializer _ ->
        State.add_initializer ~elt_kind:`Object ~elt_loc state
    | _ -> state
  in
  State.update state


let arg typ args =
  let rec collect_type_related_references typ arg =
    match get_deep_desc typ with
    | Tarrow (_, _, typ, _) ->
      collect_type_related_references typ arg
    | Tobject _ -> (
      match arg with
      | _, None -> ()
      | _, Some exp ->
        let call_site = exp.exp_loc.Location.loc_start in
        iter_fields
          (fun meth -> collect_references ~meth ~call_site exp)
          typ
      )
    | _ -> ()
  in
  let rec process_args typ args =
    match get_deep_desc typ, args with
    | Tarrow (_, t, typ, _), hd::tl ->
      collect_type_related_references t hd;
      process_args typ tl
    | _, _ -> ()
  in
  process_args typ args


let coerce expr typ =
  let loc = locate expr in
  let use meth_name =
    let state = State.get_current () in
    let use_loc = expr.exp_loc.Location.loc_start in
    let elt_kind = `Method meth_name in
    State.add_use ~elt_kind ~elt_loc:loc ~use_loc state
    |> State.update
  in
  iter_fields use typ


let eocb () =
  List.iter (fun f -> f ()) !at_eocb


                (********   WRAPPING  ********)


let wrap f x =
  let state = State.get_current () in
  if Config.must_report_section state.config.sections.methods then
    f x
  else ()

let collect_export path ?obj ?cltyp loc =
  wrap (collect_export path ~obj ~cltyp) loc

let collect_references ~meth ~call_site exp =
  wrap (collect_references ~meth ~call_site) exp

let tstr cl_dec =
  wrap tstr cl_dec

let add_var loc exp =
  wrap (add_var loc) exp

let class_structure cl_struct =
  wrap class_structure cl_struct

let class_field cl_field =
  wrap class_field cl_field

let arg typ args =
  wrap (arg typ) args
