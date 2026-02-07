(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2025 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Dead code anlyzing tool. It only reports unused exported values, constructors/record fields
  and methods by default.
  Options can enable reporting of optional arguments always/never used as bad style of code.
  In addition to selecting which reports are to be displayed, the limit of authorized
  occurences needed to be reported can be selected (default is 0).
  It assumes .mli/.mfi are compiled with -keep-locs and .ml/.mf are compiled with -bin-annot.
 *)


open Types
open Typedtree

open DeadCommon


                (********   ATTRIBUTES   ********)

let bad_files = ref []                (* unreadable cmi/cmt files *)

let main_files = Hashtbl.create 256   (* names -> paths *)


                (********   PROCESSING   ********)

let rec collect_export ?(mod_type = false) path u stock = function

  | Sig_value (id, ({Types.val_loc; val_type; _} as value), _)
    when not val_loc.Location.loc_ghost ->
      let state = State.get_current () in
      let should_export stock loc =
        Config.must_report_section state.config.sections.exported_values
        && (* do not add the loc in decs if it belongs to a module type *)
          ( stock != decs
            || not (Hashtbl.mem in_modtype loc.Location.loc_start)
          )
      in
      if should_export stock val_loc then export path u stock id val_loc;
      let path = Ident.create_persistent (Ident.name id ^ "*") :: path in
      DeadObj.collect_export path u stock ~obj:val_type val_loc;
      !DeadLexiFi.sig_value value

  | Sig_type (id, t, _, _) when stock == decs ->
      DeadType.collect_export (id :: path) u stock t

  | Sig_class (id, {Types.cty_type = t; cty_loc = loc; _}, _, _) ->
      DeadObj.collect_export (id :: path) u stock ~cltyp:t loc

  | (Sig_module (id, _, {Types.md_type = t; _}, _, _)
  | Sig_modtype (id, {Types.mtd_type = Some t; _}, _)) as s ->
      let stock =
        match s with
        | Sig_modtype _ when not mod_type -> in_modtype
        | _ -> stock
      in
      DeadMod.sign t
      |> List.iter (collect_export ~mod_type (id :: path) u stock)

  | _ -> ()


let rec treat_exp exp args =
  match exp.exp_desc with
  | Texp_apply (exp, in_args) -> treat_exp exp (in_args @ args)

  | Texp_ident (_, _, {Types.val_loc = {Location.loc_start = loc; _}; _})
  | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; _}; _}) ->
      DeadArg.register_uses loc args

  | Texp_match (_, l, _) ->
      List.iter (fun {c_rhs = exp; _} -> treat_exp exp args) l

  | Texp_ifthenelse (_, exp_then, exp_else) ->
      treat_exp exp_then args;
      begin match exp_else with
      | Some exp -> treat_exp exp args
      | _ -> ()
      end

  | _ -> ()


let value_binding super self x =
  let at_eof_saved = !DeadArg.at_eof in
  DeadArg.at_eof := [];
  incr depth;
  let open Asttypes in
  begin match x with
  | { vb_pat =
        { pat_desc = Tpat_var (
            _,
            {loc = {Location.loc_start = loc1; loc_ghost = false; _}; _}, _);
          _};
      vb_expr =
        { exp_desc = Texp_ident (
            _,
            _,
            {val_loc = {Location.loc_start = loc2; loc_ghost = false; _}; _});
          _};
      _
    } ->
      VdNode.merge_locs loc1 loc2
  | { vb_pat =
        { pat_desc = Tpat_var (
            _,
            {loc = {Location.loc_start = loc; loc_ghost = false; _}; _}, _);
          _};
      vb_expr = exp;
      _
    } ->
      DeadArg.bind loc exp;
      DeadObj.add_var loc exp
  | _ -> ()
  end;

  let r = super.Tast_mapper.value_binding self x in
  List.iter (fun f -> f()) !DeadArg.at_eof;
  DeadArg.at_eof := at_eof_saved;
  decr depth;
  r


let structure_item super self i =
  let state = State.get_current () in
  let sections = state.config.sections in
  let open Asttypes in
  begin match i.str_desc with
  | Tstr_type  (_, l) when Config.must_report_section sections.types ->
      List.iter DeadType.tstr l
  | Tstr_module  {mb_name = {txt = Some txt; _}; _} ->
      mods := txt :: !mods;
      DeadMod.defined := String.concat "." (List.rev !mods) :: !DeadMod.defined
  | Tstr_class l when Config.must_report_section sections.methods ->
      List.iter DeadObj.tstr l
  | Tstr_include i ->
      let collect_include signature =
        let prev_last_loc = !last_loc in
        let module_id =
          State.File_infos.get_modname state.file_infos
          |> Ident.create_persistent
        in
        List.iter
          (collect_export ~mod_type:true [module_id] _include incl)
          signature;
        last_loc := prev_last_loc;
      in
      let rec includ mod_expr =
        match mod_expr.mod_desc with
        | Tmod_ident (_, _) -> collect_include (DeadMod.sign mod_expr.mod_type)
        | Tmod_structure structure -> collect_include structure.str_type
        | Tmod_unpack (_, mod_type) -> collect_include (DeadMod.sign mod_type)
        | Tmod_functor (_, mod_expr)
        | Tmod_apply (_, mod_expr, _)
        | Tmod_apply_unit mod_expr
        | Tmod_constraint (mod_expr, _, _, _) -> includ mod_expr
      in
      includ i.incl_mod
  | _ -> ()
  end;
  let r = super.Tast_mapper.structure_item self i in
  begin match i.str_desc with
  | Tstr_module _ -> mods := List.tl !mods
  | _ -> ()
  end;
  r


let pat: type k. Tast_mapper.mapper -> Tast_mapper.mapper -> k general_pattern -> k general_pattern =
 fun super self p ->
  let state = State.get_current () in
  let sections = state.config.sections in
  let pat_loc = p.pat_loc.Location.loc_start in
  let u s =
    register_style pat_loc (Printf.sprintf "unit pattern %s" s)
  in
  let open Asttypes in
  if DeadType.is_unit p.pat_type && sections.style.unit_pat then begin
    match p.pat_desc with
      | Tpat_construct _ -> ()
      | Tpat_var (_, {txt = "eta"; loc = _}, _)
        when p.pat_loc = Location.none -> ()
      | Tpat_var (_, {txt; _}, _) -> if check_underscore txt then u txt
      | Tpat_any -> if state.config.underscore then u "_"
      | Tpat_value tpat_arg ->
        begin match (tpat_arg :> value general_pattern) with
        | {pat_desc=Tpat_construct _; _} -> ()
        | _ -> u "other"
        end
      | _ -> u ""
  end;
  begin match p.pat_desc with
  | Tpat_record (l, _) ->
      List.iter
        (fun (_, {Types.lbl_loc = {Location.loc_start = lab_loc; _}; _}, _) ->
           if exported ~is_type:true sections.types lab_loc then
            DeadType.collect_references lab_loc pat_loc
        )
        l
  | _ -> ()
  end;
  super.Tast_mapper.pat self p


let expr super self e =
  let state = State.get_current () in
  let sections = state.config.sections in
  let rec extra = function
    | [] -> ()
    | (Texp_coerce (_, typ), _, _)::l -> DeadObj.coerce e typ.ctyp_type; extra l
    | _::l -> extra l
  in
  extra e.exp_extra;
  let exp_loc = e.exp_loc.Location.loc_start in
  begin match e.exp_desc with

  | Texp_ident (path, _, _) when Path.name path = "Mlfi_types.internal_ttype_of" ->
      !DeadLexiFi.ttype_of e

  | Texp_ident (_, _, {Types.val_loc = {Location.loc_start = loc; loc_ghost = false; _}; _})
    when exported sections.exported_values loc ->
      LocHash.add_set references loc exp_loc

  | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; loc_ghost = false; _}; _})
  | Texp_construct (_, {cstr_loc = {Location.loc_start = loc; loc_ghost = false; _}; _}, _)
    when exported ~is_type:true sections.types loc ->
      DeadType.collect_references loc exp_loc

  | Texp_send (e2, Tmeth_name meth) ->
    DeadObj.collect_references ~meth ~call_site:e.exp_loc.Location.loc_start e2
  | Texp_send (e2, Tmeth_val id)
  | Texp_send (e2, Tmeth_ancestor (id, _)) ->
    DeadObj.collect_references ~meth:(Ident.name id) ~call_site:e.exp_loc.Location.loc_start e2


  | Texp_apply (exp, args) ->
      if Config.must_report_opt_args state.config then
        treat_exp exp args;
      begin match exp.exp_desc with
      | Texp_ident (_, _, {Types.val_loc; _})
        when val_loc.Location.loc_ghost -> (* The node is due to lookup preparation
            * anticipated in the typedtree, wich is a case we do not want to treat
            * otherwise all object's content would be marked as used at this point... *)
          ()
      | Texp_ident (_, _, {val_type; _}) ->
          DeadObj.arg val_type args
      | _ ->
          DeadObj.arg exp.exp_type args
      end

  | Texp_let (_, [{vb_pat; _}], _)
    when DeadType.is_unit vb_pat.pat_type && sections.style.seq ->
      begin match vb_pat.pat_desc with
      | Tpat_var (id, _, _) when not (check_underscore (Ident.name id)) -> ()
      | _ ->
          register_style
            vb_pat.pat_loc.Location.loc_start
            "let () = ... in ... (=> use sequence)"
      end

  | Texp_match (_, [{c_lhs; _}], _)
    when DeadType.is_unit c_lhs.pat_type && sections.style.seq ->
      begin match c_lhs.pat_desc with
      | Tpat_value tpat_arg ->
        begin match (tpat_arg :> value general_pattern) with
        | {pat_desc=Tpat_construct _; _} ->
            register_style
              c_lhs.pat_loc.Location.loc_start
              "let () = ... in ... (=> use sequence)"
        | _ -> ()
        end
      | _ -> ()
      end

  | Texp_let (
        Asttypes.Nonrecursive,
        [{vb_pat = {pat_desc = Tpat_var (id1, _, _); pat_loc = {loc_start = loc; _}; _}; _}],
        {exp_desc = Texp_ident (Path.Pident id2, _, _); exp_extra = []; _})
    when id1 = id2
         && sections.style.binding
         && check_underscore (Ident.name id1) ->
      register_style loc "let x = ... in x (=> useless binding)"

  | _ -> ()
  end;
  super.Tast_mapper.expr self e


(* Parse the AST *)
let collect_references =                          (* Tast_mapper *)
  let super = Tast_mapper.default in
  let wrap f loc self x =
    let l = !last_loc in
    let ll = (loc x).Location.loc_start in
    if ll <> Lexing.dummy_pos then last_loc := ll;
    let prev_last_class = !DeadObj.last_class in
    let r = f self x in
    DeadObj.last_class := prev_last_class;
    last_loc := l;
    r
  in

  let expr = wrap (expr super) (fun x -> x.exp_loc) in
  let pat: 'k. Tast_mapper.mapper -> 'k general_pattern -> 'k general_pattern =
    fun m p -> wrap (pat super) (fun x -> x.pat_loc) m p
  in
  let structure_item = wrap (structure_item super) (fun x -> x.str_loc) in
  let value_binding = wrap (value_binding super) (fun x -> x.vb_expr.exp_loc) in
  let module_expr =
    wrap
      (fun self x -> DeadMod.expr x; super.Tast_mapper.module_expr self x)
      (fun x -> x.mod_loc)
  in
  let class_structure =
    (fun self x ->
     DeadObj.class_structure x; super.Tast_mapper.class_structure self x)
  in
  let class_field =
    (fun self x ->
     DeadObj.class_field x;
     super.Tast_mapper.class_field self x)
  in
  let class_field = wrap class_field (fun x -> x.cf_loc) in
  let typ =
    (fun self x ->
     !DeadLexiFi.type_ext x; super.Tast_mapper.typ self x)
  in
  let type_declaration self x =
    !DeadLexiFi.type_decl x;
    super.Tast_mapper.type_declaration self x
  in
  Tast_mapper.{ super with
                structure_item; expr; pat; value_binding;
                module_expr; class_structure; class_field; typ;
                type_declaration
              }


let regabs state =
  let fn = State.File_infos.get_sourcepath state.State.file_infos in
  hashtbl_add_unique_to_list abspath (Utils.unit fn) fn;
  if !DeadCommon.declarations then
    hashtbl_add_unique_to_list main_files (Utils.unit fn) ()


let read_interface fn cmi_infos state = let open Cmi_format in
  try
    regabs state;
    if Config.must_report_main state.config then
      let u =
        if State.File_infos.has_sourcepath state.file_infos then
          State.File_infos.get_sourceunit state.file_infos
        else
        Utils.unit fn
      in
      let module_id =
        State.File_infos.get_modname state.file_infos
        |> Ident.create_persistent
      in
      let f =
        collect_export [module_id] u decs
      in
      List.iter f cmi_infos.cmi_sign;
      last_loc := Lexing.dummy_pos
  with Cmi_format.Error (Wrong_version_interface _) ->
    (*Printf.eprintf "cannot read cmi file: %s\n%!" fn;*)
    bad_files := fn :: !bad_files


(* Merge a location's references to another one's *)
let assoc decs (loc1, loc2) =
  let state = State.get_current () in
  let fn1 = loc1.Lexing.pos_fname
  and fn2 = loc2.Lexing.pos_fname in
  let sourceunit = State.File_infos.get_sourceunit state.file_infos in
  let is_implem fn = fn.[String.length fn - 1] <> 'i' in
  let has_iface fn =
    fn.[String.length fn - 1] = 'i'
    || ( Utils.unit fn = sourceunit
      && DeadCommon.file_exists (fn ^ "i"))
  in
  let is_iface fn loc =
    Hashtbl.mem decs loc || Utils.unit fn <> sourceunit
    || not (is_implem fn && has_iface fn)
  in
  if fn1 <> _none && fn2 <> _none && loc1 <> loc2 then begin
    if (state.config.internal || fn1 <> fn2) && is_implem fn1 && is_implem fn2 then
      DeadCommon.LocHash.merge_set references loc2 references loc1;
    if is_iface fn1 loc1 then begin
      if is_iface fn2 loc2 then
        DeadCommon.LocHash.add_set references loc1 loc2
      else
        DeadCommon.LocHash.merge_set references loc1 references loc2;
    end
    else
      DeadCommon.LocHash.merge_set references loc2 references loc1
  end


let clean references loc =
  let state = State.get_current () in
  let sourceunit = State.File_infos.get_sourceunit state.file_infos in
  let fn = loc.Lexing.pos_fname in
  if (fn.[String.length fn - 1] <> 'i' && Utils.unit fn = sourceunit) then
    LocHash.remove references loc

let eof loc_dep =
  let state = State.get_current () in
  DeadArg.eof();
  List.iter (assoc decs) loc_dep;
  List.iter (assoc DeadType.decs) !DeadType.dependencies;
  let sourcepath = State.File_infos.get_sourcepath state.State.file_infos in
  if DeadCommon.file_exists (sourcepath ^ "i") then begin
    let clean =
      List.iter
        (fun (loc1, loc2) ->
          clean references loc1; clean references loc2
        )
    in
    clean loc_dep;
    clean !DeadType.dependencies;
  end;
  VdNode.eof ();
  DeadObj.eof ();
  DeadType.dependencies := [];
  Hashtbl.reset incl


(* Starting point *)
let rec load_file fn state =
  let init_and_continue state fn f =
    match State.change_file state fn with
    | Error msg ->
      Printf.eprintf "%s\n%!" msg;
      state
    | Ok state ->
        State.update state;
        f state;
        (* TODO: stateful computations should take and return the state when possible *)
        state
  in
  let exclude filepath = Config.is_excluded filepath state.State.config in
  match Utils.kind ~exclude fn with
  | `Cmi when !DeadCommon.declarations ->
      last_loc := Lexing.dummy_pos;
      if state.State.config.verbose then Printf.eprintf "Scanning %s\n%!" fn;
      init_and_continue state fn (fun state ->
      match state.file_infos.cmi_infos with
      | None -> () (* TODO error handling ? *) 
      | Some cmi_infos -> read_interface fn cmi_infos state
      )

  | `Cmt ->
      let open Cmt_format in
      last_loc := Lexing.dummy_pos;
      if state.config.verbose then Printf.eprintf "Scanning %s\n%!" fn;
      init_and_continue state fn (fun state ->
      regabs state;
      match state.file_infos.cmt_infos with
      | None -> bad_files := fn :: !bad_files
      | Some {cmt_annots = Implementation x; cmt_value_dependencies; _} ->
          let prepare = function
            | {Types.val_loc = {Location.loc_start = loc1; loc_ghost = false; _}; _},
              {Types.val_loc = {Location.loc_start = loc2; loc_ghost = false; _}; _} ->
                DeadObj.add_equal loc1 loc2;
                VdNode.merge_locs ~force:true loc2 loc1
            | _ -> ()
          in
          List.iter prepare cmt_value_dependencies;

          ignore (collect_references.Tast_mapper.structure collect_references x);

          let loc_dep =
            if Config.must_report_section state.config.sections.exported_values then
              List.rev_map
                (fun (vd1, vd2) ->
                  (vd1.Types.val_loc.Location.loc_start, vd2.Types.val_loc.Location.loc_start)
                )
                cmt_value_dependencies
            else []
          in
          eof loc_dep
      | _ -> () (* todo: support partial_implementation? *)
      )

  | `Dir ->
      let next = Sys.readdir fn in
      Array.sort compare next;
      Array.fold_left
        (fun state s -> load_file (fn ^ "/" ^ s) state)
        state
        next
      (* else Printf.eprintf "skipping directory %s\n" fn *)

  | _ -> state


                (********   REPORTING   ********)

(* Prepare the list of opt_args for report *)
let analyze_opt_args () =
  DeadArg.eocb ();
  let dec_loc loc = Hashtbl.mem main_files (Utils.unit loc.Lexing.pos_fname) in
  let all = ref [] in
  let opt_args_tbl = Hashtbl.create 256 in

  let analyze = fun opt_arg_use ->
    let builddir = opt_arg_use.builddir in
    let loc = opt_arg_use.decl_loc in
    let lab = opt_arg_use.label in
    let slot =
      try Hashtbl.find opt_args_tbl (builddir, loc, lab)
      with Not_found ->
        let r = {with_val = []; without_val = []} in
        if dec_loc loc then begin
          all := (builddir, loc, lab, r) :: !all;
          Hashtbl.add opt_args_tbl (builddir, loc, lab) r
        end;
        r
    in
    let call_site = opt_arg_use.use_loc in
    if opt_arg_use.has_val then slot.with_val <- call_site :: slot.with_val
    else slot.without_val <- call_site :: slot.without_val
  in

  List.iter analyze !opt_args;
  List.iter                   (* Remove call sites accounted more than once for the same element *)
    (fun (_, _, _, slot) ->
      slot.with_val     <- List.sort_uniq compare slot.with_val;
      slot.without_val  <- List.sort_uniq compare slot.without_val)
    !all;
  !all


let report_opt_args s l =
  let state = State.get_current () in
  let opt =
    if s = "NEVER" then state.config.sections.optn
    else state.config.sections.opta
  in
  let rec report_opt_args nb_call =
    let l = List.filter
        (fun (_, _, _, slot, ratio, _) -> let ratio = 1. -. ratio in
          match  opt with
          | Off ->
              (* TODO: better error handling *)
              failwith "Trying to report a deactivated opt args section"
          | On -> ratio >= 1. && nb_call = 0
          | Threshold {threshold = Both (_, percentage); _} ->
              ratio >= percentage && check_length nb_call slot
          | Threshold {threshold = Percent percentage as threshold; _} ->
              let percent = percent threshold in

              ratio >= percent nb_call
              && (percentage >= 1. || ratio < (percent (nb_call - 1))))
      @@ List.map
        (fun (builddir, loc, lab, slot) ->
          let l = if s = "NEVER" then slot.with_val else slot.without_val in
          let total = List.length slot.with_val + List.length slot.without_val in
          let ratio = float_of_int (List.length l) /. float_of_int total
          in (builddir, loc, lab, l, ratio, total))
        l
      |> List.fast_sort (fun (builddir1, loc1, lab1, slot1, _, _) (builddir2, loc2, lab2, slot2, _, _) ->
          let fn1 = Filename.concat builddir1 loc1.Lexing.pos_fname in
          let fn2 = Filename.concat builddir2 loc2.Lexing.pos_fname in
          compare (fn1, loc1, lab1, slot1) (fn2, loc2, lab2, slot2))
    in

    let change =
      let loc =
        match l with
        | (_, loc, _, _, _, _)::_ -> loc
        | _ -> !last_loc
      in
      dir (DeadCommon.abs loc)
    in

    let pretty_print = fun (builddir, loc, lab, slot, ratio, total) ->
      if change (DeadCommon.abs loc) then print_newline ();
      let fn = Filename.concat builddir loc.Lexing.pos_fname in
      prloc ~fn loc; print_string ("?" ^ lab);
      if ratio <> 0. then begin
        Printf.printf "   (%d/%d calls)" (total - List.length slot) total;
        if Config.must_report_call_sites opt then print_string "  Exceptions:"
      end;
      print_newline ();
      if Config.must_report_call_sites opt then begin
        List.iter (pretty_print_call ()) slot;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call =
      match opt with
      | Off | On -> false
      | Threshold {threshold = Both (exceptions, _); _} ->
          nb_call < exceptions
      | Threshold {threshold = Percent percentage as threshold; _} ->
          percent threshold nb_call > percentage
    in
    let s =
      (if nb_call > 0 then "OPTIONAL ARGUMENTS: ALMOST "
      else "OPTIONAL ARGUMENTS: ") ^ s
    in
    report s ~opt ~extra:"Except" l continue nb_call pretty_print report_opt_args;

  in report_opt_args 0


let report_unused_exported () =
  let state = State.get_current () in
  report_basic
    decs
    "UNUSED EXPORTED VALUES"
    state.config.sections.exported_values


let report_style () =
  section "CODING STYLE";
  if !style <> [] then begin
    style := List.fast_sort compare !style;
    let change =
      let (fn, _, _) = List.hd !style in
      dir fn
    in
    List.iter (fun (fn, l, s) ->
      if change fn then print_newline ();
      prloc ~fn l;
      print_endline s)
    !style;
  end;
  print_newline ()
  |> separator


(* Option parsing and processing *)
let run_analysis state =
  let process_file filename state =
    let state = load_file filename state in
    State.update state;
    state
  in
  Printf.eprintf "Scanning files...\n%!";
  Utils.StringSet.fold
    process_file
    state.State.config.paths_to_analyze
    state

let () =
try
    let config = Config.parse_cli () in
    let state = State.init config in
    let state = run_analysis state in
    let run_on_references_only state =
      DeadCommon.declarations := false;
      let no_style_config = Config.update_style "-all" state.State.config in
      let state = State.update_config no_style_config state in
      let state =
        Utils.StringSet.fold
          load_file
          state.config.references_paths
          state
      in
      State.update_config config state
    in
    let state = run_on_references_only state in
    State.update state;

    Printf.eprintf " [DONE]\n\n%!";

    !DeadLexiFi.prepare_report DeadType.decs;
    let sections = state.config.sections in
    if Config.must_report_section sections.exported_values then report_unused_exported ();
    DeadObj.report();
    DeadType.report();
    if Config.must_report_opt_args state.config then begin
      let tmp = analyze_opt_args () in
      if Config.must_report_section sections.opta then report_opt_args "ALWAYS" tmp;
      if Config.must_report_section sections.optn then report_opt_args "NEVER" tmp
    end;
    let style = sections.style in
    if style.opt_arg || style.unit_pat || style.seq || style.binding then
      report_style ();

    if !bad_files <> [] then begin
      let oc = open_out_bin "remove_bad_files.sh" in
      Printf.fprintf oc "#!/bin/sh\n";
      List.iter
        (fun x -> Printf.fprintf oc "rm %s\n" x)
        !bad_files;
      close_out oc;
      Printf.eprintf "*** INFO: Several binary files cannot be read.  Please run ./remove_bad_files.sh to remove them.\n%!"
    end
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
