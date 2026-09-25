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

let decs = Hashtbl.create 256

let at_eof = ref []

let last_class = ref Lexing.dummy_pos            (* last class met *)

let defined = Hashtbl.create 16



                (********   HELPERS   ********)


let repr_loc obj_loc =
  let state = State.get_current () in
  State.Methods.get_orig_loc ~obj_loc state.methods


let add_path obj_path obj_loc =
  let state = State.get_current () in
  State.Methods.add_loc_binding ~obj_path ~obj_loc state.methods
  |> ignore


let get_loc path =
  let path =
    let exported_path =
      Hashtbl.to_seq_values incl
      |> Seq.find (fun (_, exported_path) -> is_sub_path ~sep:"." path exported_path)
    in
    match exported_path with
    | Some (_, exported_path) -> exported_path
    | None ->
      try Hashtbl.find defined path
      with Not_found -> path
  in
  let state = State.get_current () in
  State.Methods.find_loc ~obj_path:path state.methods
  |> Option.map repr_loc


let add_equal loc1 loc2 =
  let loc1 = repr_loc loc1
  and loc2 = repr_loc loc2 in
  if loc1 <> loc2 && not (is_ghost loc1 || is_ghost loc2) then begin
    if loc1 = !last_class then
      last_class := loc2;
    let state = State.get_current () in
    State.Methods.add_alias ~orig_loc:loc2 ~alias_loc:loc1 state.methods
    |> ignore
  end



let rec treat_fields action typ = match get_deep_desc typ with
  | Tobject (t, _)
  | Tarrow (_, _, t, _) -> treat_fields action t
  | Tfield (s, k, _, t) ->
      if field_kind_repr k <> Fabsent && s.[0] > 'Z' then
        action s;
      treat_fields action t
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
        repr_loc loc.Location.loc_start
    | _ -> Lexing.dummy_pos
  in repr_exp expr locate


let eof () =
  Hashtbl.reset defined;
  last_class := Lexing.dummy_pos



                (********   PROCESSING  ********)


let collect_export path u stock ~obj ~cltyp loc =

  let pos = loc.Location.loc_start in

  begin match List.rev path with
  | h :: t ->
      let short = String.concat "." t in
      let path = h ^ "." ^ short in
      Hashtbl.add defined short path;
      add_path path pos
  | _ -> ()
  end;

  let stock =
    if stock == DeadCommon.decs then decs
    else begin
      export (List.tl path) u stock (List.hd path) loc;
      stock
    end
  in

  let save id =
    let state = State.get_current () in
    let sourcepath = State.File_infos.get_sourcepath state.State.file_infos in
    (* TODO: resolve the builddir ('/workspace_root') in the case of dune
       compiled projects. Without it, looking up for an existing csml below will
       always fail and can lead to false positive *)
    if not (Sys.file_exists (Filename.remove_extension sourcepath ^ ".csml")) then begin
      if stock == DeadCommon.incl then
        export ~sep:"#" path u stock id loc
      else
        let meth_path =
          String.concat "." (List.rev path)
          ^ "#" ^ id
        in
        let obj_loc = loc.Location.loc_start in
        let builddir = State.File_infos.get_builddir state.file_infos in
        State.Methods.add_exported_declaration ~obj_loc ~meth_name:id ~builddir ~meth_path state.methods
        |> ignore
    end
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
        treat_fields save typ
    | None -> ()

let correct_export loc =
  let state = State.get_current () in
  let builddir = State.File_infos.get_builddir state.file_infos in
  let obj_loc = loc.Location.loc_start in
  State.Methods.remove_exported_declarations ~obj_loc ~builddir state.methods
  |> ignore


let collect_references ~meth ~call_site expr =
  let obj_loc = locate expr in

  if not (is_ghost obj_loc) then begin
    let state = State.get_current () in
    let meth_name = meth in
    let use_loc = call_site in
    State.Methods.add_use ~obj_loc ~meth_name ~use_loc state.methods
    |> ignore;
    if obj_loc = !last_class then
      State.Methods.add_self_use ~obj_loc ~meth_name ~use_loc state.methods
      |> ignore
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
  if not (Hashtbl.mem defined short) then
    Hashtbl.add defined short path
  else begin
    (* using begin ... end because otherwise make_dep below is considered
       part of this else *)
    let loc =
      match get_loc short with
      | None -> get_loc path
      | some -> some
    in
    match loc with
    | Some loc when loc <> !last_class -> add_equal !last_class loc
    | _ -> add_path path !last_class
  end;

  let rec make_dep ci_expr =
    match ci_expr.cl_desc with
    | Tcl_ident (path, _, _) ->
        get_loc (Path.name path)
        |> Option.iter (add_equal !last_class)
    | Tcl_fun (_, _, _, ci_expr, _)
    | Tcl_constraint (ci_expr, _, _, _, _) -> make_dep ci_expr
    | _ -> ()
  in
  make_dep ci_expr


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
  match repr_exp expr kind with
  | `Obj ->
      last_class := loc;
  | `New cty_loc -> add_equal loc cty_loc
  | `Ident id_loc ->
      let expr_loc : Location.t = expr.exp_loc in
      if id_loc >= expr_loc.loc_start && id_loc <= expr_loc.loc_end then
        (* ident is defined within expr *)
        add_equal id_loc loc
  | `Ignore -> ()


let class_structure cl_struct =
  let rec add_aliases pat =
    begin match pat.pat_desc with
    | Tpat_alias _
    | Tpat_var _ when not pat.pat_loc.Location.loc_ghost ->
        add_equal pat.pat_loc.Location.loc_start !last_class
    | _ -> () end;
    match Utils.Compat.get_alias_data pat.pat_desc with
    | Ok (pat, _, _, _) -> add_aliases pat
    | _ -> ()
  in
  add_aliases cl_struct.cstr_self


let class_field f =
  let rec locate cl_exp = match cl_exp.cl_desc with
    | Tcl_ident (path, _, _) ->
        let path = Path.name path in
        if Hashtbl.mem defined path then Hashtbl.find defined path
        else path
    | Tcl_fun (_, _, _, cl_exp, _)
    | Tcl_apply (cl_exp, _)
    | Tcl_let (_, _, _, cl_exp)
    | Tcl_constraint (cl_exp, _, _, _, _) -> locate cl_exp
    | Tcl_structure _ | Tcl_open _ -> _none
  in
  match f.cf_desc with
  | Tcf_inherit (_, cl_exp, _, _, l) ->
      let path = locate cl_exp in
      if path != _none then begin
        let state = State.get_current () in
        let builddir = State.File_infos.get_builddir state.file_infos in
        List.iter
          (fun (meth_name, _) ->
            State.Methods.mark_inherited
              ~builddir ~obj_loc:!last_class ~meth_name
              ~inherited_path:path
              state.methods
            |> ignore
          )
          l;
        State.Methods.inherit_initializer
          ~builddir ~obj_loc:!last_class
          ~inherited_path:path state.methods
        |> ignore;
        add_equal f.cf_loc.Location.loc_start cl_exp.cl_loc.Location.loc_start;
        match get_loc path with
        | None ->
            (* path has not been located yet. Try again later. *)
            let equal () =
              get_loc path
              |> Option.iter (add_equal cl_exp.cl_loc.Location.loc_start)
            in
            at_eof := equal :: !at_eof
        | Some loc ->
            add_equal cl_exp.cl_loc.Location.loc_start loc
      end

  | Tcf_method ({txt; _}, _, Tcfk_virtual _) ->
      let state = State.get_current () in
      let builddir = State.File_infos.get_builddir state.file_infos in
      State.Methods.mark_virtual ~builddir ~obj_loc:!last_class ~meth_name:txt state.methods
      |> ignore
  | Tcf_method ({txt; _}, _, _) ->
      let state = State.get_current () in
      let builddir = State.File_infos.get_builddir state.file_infos in
      State.Methods.mark_defined ~builddir ~obj_loc:!last_class ~meth_name:txt state.methods
      |> ignore

  | Tcf_initializer _ ->
      let state = State.get_current () in
      let builddir = State.File_infos.get_builddir state.file_infos in
      State.Methods.add_initializer
        ~builddir ~obj_loc:!last_class state.methods
      |> ignore

  | _ -> ()


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
        treat_fields
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
    State.Methods.add_use ~obj_loc:loc ~meth_name ~use_loc state.methods
    |> ignore
  in
  treat_fields use typ


let prepare_report () =
  List.iter (fun f -> f ()) !at_eof;
  let state = State.get_current () in
  State.Methods.resolve_aliases state.methods
  |> State.Methods.resolve_inheritances
  |> ignore


let report () =
  prepare_report ();
  let state = State.get_current () in
  Report.report state `Method



                (********   WRAPPING  ********)


let wrap f x =
  let state = State.get_current () in
  if Config.must_report_section state.config.sections.methods then
    f x
  else ()

let collect_export path u stock ?obj ?cltyp loc =
  wrap (collect_export path u stock ~obj ~cltyp) loc

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

let report () =
  wrap report ()
