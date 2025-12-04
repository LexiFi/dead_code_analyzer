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
    when not val_loc.Location.loc_ghost && stock == decs ->
      if !DeadFlag.exported.DeadFlag.print then export path u stock id val_loc;
      let path = Ident.create_persistent (Ident.name id ^ "*") :: path in
      DeadObj.collect_export path u stock ~obj:val_type val_loc;
      !DeadLexiFi.sig_value value

  | Sig_type (id, t, _, _) when stock == decs ->
      DeadType.collect_export (id :: path) u stock t

  | Sig_class (id, {Types.cty_type = t; cty_loc = loc; _}, _, _) ->
      DeadObj.collect_export (id :: path) u stock ~cltyp:t loc

  | (Sig_module (id, _, {Types.md_type = t; _}, _, _)
  | Sig_modtype (id, {Types.mtd_type = Some t; _}, _)) as s ->
      let collect = match s with Sig_modtype _ -> mod_type | _ -> true in
      if collect then
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
  let open Asttypes in
  begin match i.str_desc with
  | Tstr_type  (_, l) when !DeadFlag.typ.DeadFlag.print ->
      List.iter DeadType.tstr l
  | Tstr_module  {mb_name = {txt = Some txt; _}; _} ->
      mods := txt :: !mods;
      DeadMod.defined := String.concat "." (List.rev !mods) :: !DeadMod.defined
  | Tstr_class l when !DeadFlag.obj.DeadFlag.print -> List.iter DeadObj.tstr l
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
  let pat_loc = p.pat_loc.Location.loc_start in
  let u s =
    register_style pat_loc (Printf.sprintf "unit pattern %s" s)
  in
  let open Asttypes in
  if DeadType.is_unit p.pat_type && !DeadFlag.style.DeadFlag.unit_pat then begin
    match p.pat_desc with
      | Tpat_construct _ -> ()
      | Tpat_var (_, {txt = "eta"; loc = _}, _)
        when p.pat_loc = Location.none -> ()
      | Tpat_var (_, {txt; _}, _) -> if check_underscore txt then u txt
      | Tpat_any -> if not !DeadFlag.underscore then u "_"
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
          if exported DeadFlag.typ lab_loc then
            DeadType.collect_references lab_loc pat_loc
        )
        l
  | _ -> ()
  end;
  super.Tast_mapper.pat self p


let expr super self e =
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
    when exported DeadFlag.exported loc ->
      LocHash.add_set references loc exp_loc

  | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; loc_ghost = false; _}; _})
  | Texp_construct (_, {cstr_loc = {Location.loc_start = loc; loc_ghost = false; _}; _}, _)
    when exported DeadFlag.typ loc ->
      DeadType.collect_references loc exp_loc

  | Texp_send (e2, Tmeth_name meth) ->
    DeadObj.collect_references ~meth ~call_site:e.exp_loc.Location.loc_start e2
  | Texp_send (e2, Tmeth_val id)
  | Texp_send (e2, Tmeth_ancestor (id, _)) ->
    DeadObj.collect_references ~meth:(Ident.name id) ~call_site:e.exp_loc.Location.loc_start e2


  | Texp_apply (exp, args) ->
      if DeadFlag.(!opta.print || !optn.print) then treat_exp exp args;
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
    when DeadType.is_unit vb_pat.pat_type && !DeadFlag.style.DeadFlag.seq ->
      begin match vb_pat.pat_desc with
      | Tpat_var (id, _, _) when not (check_underscore (Ident.name id)) -> ()
      | _ ->
          register_style
            vb_pat.pat_loc.Location.loc_start
            "let () = ... in ... (=> use sequence)"
      end

  | Texp_match (_, [{c_lhs; _}], _)
    when DeadType.is_unit c_lhs.pat_type && !DeadFlag.style.DeadFlag.seq ->
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
         && !DeadFlag.style.DeadFlag.binding
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

(* Checks the nature of the file *)
let kind fn =
  if not (Sys.file_exists fn) then begin
    prerr_endline ("Warning: '" ^ fn ^ "' not found");
    `Ignore
  end else if DeadFlag.is_excluded fn then `Ignore
  else if Sys.is_directory fn then `Dir
  else if Filename.check_suffix fn ".cmi" then `Cmi
  else if Filename.check_suffix fn ".cmt" then `Cmt
  else `Ignore


let regabs state =
  let fn = State.File_infos.get_sourcepath state.State.file_infos in
  hashtbl_add_unique_to_list abspath (Utils.unit fn) fn;
  if !DeadCommon.declarations then
    hashtbl_add_unique_to_list main_files (Utils.unit fn) ()


let read_interface fn cmi_infos state = let open Cmi_format in
  try
    regabs state;
    if !DeadFlag.exported.DeadFlag.print
       || !DeadFlag.obj.DeadFlag.print
       || !DeadFlag.typ.DeadFlag.print
    then
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
    if (!DeadFlag.internal || fn1 <> fn2) && is_implem fn1 && is_implem fn2 then
      DeadCommon.LocHash.merge_set references loc2 references loc1;
    if is_iface fn1 loc1 then begin
      DeadCommon.LocHash.merge_set references loc1 references loc2;
      if is_iface fn2 loc2 then
        DeadCommon.LocHash.add_set references loc1 loc2
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
let rec load_file state fn =
  let init_and_continue state fn f =
    match State.change_file state fn with
    | Error msg ->
      Printf.eprintf "%s\n!" msg;
      state
    | Ok state ->
        State.update state;
        f state;
        (* TODO: stateful computations should take and return the state when possible *)
        state
  in
  match kind fn with
  | `Cmi when !DeadCommon.declarations ->
      last_loc := Lexing.dummy_pos;
      if !DeadFlag.verbose then Printf.eprintf "Scanning %s\n%!" fn;
      init_and_continue state fn (fun state ->
      match state.file_infos.cmi_infos with
      | None -> () (* TODO error handling ? *) 
      | Some cmi_infos -> read_interface fn cmi_infos state
      )

  | `Cmt ->
      let open Cmt_format in
      last_loc := Lexing.dummy_pos;
      if !DeadFlag.verbose then Printf.eprintf "Scanning %s\n%!" fn;
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
            if !DeadFlag.exported.DeadFlag.print then
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
        (fun state s -> load_file state (fn ^ "/" ^ s))
        state
        next
      (* else Printf.eprintf "skipping directory %s\n" fn *)

  | _ -> state


                (********   REPORTING   ********)

(* Prepare the list of opt_args for report *)
let analyze_opt_args () =
  DeadArg.eocb ();
  let all = ref [] in
  let tbl = Hashtbl.create 256 in
  let dec_loc loc = Hashtbl.mem main_files (Utils.unit loc.Lexing.pos_fname) in

  let analyze = fun opt_arg_use ->
    let builddir = opt_arg_use.builddir in
    let loc = opt_arg_use.decl_loc in
    let lab = opt_arg_use.label in
    let slot =
      try Hashtbl.find tbl (loc, lab)
      with Not_found ->
        let r = {with_val = []; without_val = []} in
        if dec_loc loc then begin
          all := (builddir, loc, lab, r) :: !all;
          Hashtbl.add tbl (loc, lab) r
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
  let opt =
    if s = "NEVER" then !DeadFlag.optn
    else !DeadFlag.opta
  in
  let percent = percent opt in
  let rec report_opt_args nb_call =
    let open DeadFlag in
    let l = List.filter
        (fun (_, _, _, slot, ratio, _) -> let ratio = 1. -. ratio in
          if opt.threshold.optional = `Both then
            ratio >= opt.threshold.percentage && check_length nb_call slot
          else ratio >= percent nb_call
            && (opt.threshold.percentage >= 1. || ratio < (percent (nb_call - 1))))
      @@ List.map
        (fun (builddir, loc, lab, slot) ->
          let l = if s = "NEVER" then slot.with_val else slot.without_val in
          let total = List.length slot.with_val + List.length slot.without_val in
          let ratio = float_of_int (List.length l) /. float_of_int total
          in (builddir, loc, lab, l, ratio, total))
        l
      |> List.fast_sort (fun (_, loc1, lab1, slot1, _, _) (_, loc2, lab2, slot2, _, _) ->
          compare (DeadCommon.abs loc1, loc1, lab1, slot1) (DeadCommon.abs loc2, loc2, lab2, slot2))
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
        if opt.call_sites then print_string "  Exceptions:"
      end;
      print_newline ();
      if opt.call_sites then begin
        List.iter (pretty_print_call ()) slot;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call =
      opt.threshold.optional = `Both && nb_call < opt.threshold.exceptions
      || opt.threshold.optional = `Percent && percent nb_call > opt.threshold.percentage
    in
    let s =
      (if nb_call > 0 then "OPTIONAL ARGUMENTS: ALMOST "
      else "OPTIONAL ARGUMENTS: ") ^ s
    in
    report s ~opt ~extra:"Except" l continue nb_call pretty_print report_opt_args;

  in report_opt_args 0


let report_unused_exported () = report_basic decs "UNUSED EXPORTED VALUES" !DeadFlag.exported


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
let parse () =
  let update_all print () =
    DeadFlag.(
    update_style ((if print = "all" then "+" else "-") ^ "all");
    update_basic "-E" DeadFlag.exported print;
    update_basic "-M" obj print;
    update_basic "-T" typ print;
    update_opt opta print;
    update_opt optn print)
  in

  let load_file filename =
    let state = State.get_current () in
    let state = load_file state filename in
    State.update state
  in

  (* any extra argument can be accepted by any option using some
   * although it doesn't necessary affects the results (e.g. -O 3+4) *)
  Arg.(parse
    [ "--exclude", String DeadFlag.exclude, "<path>  Exclude given path from research.";

      "--references",
        String (fun dir -> DeadFlag.directories := dir :: !DeadFlag.directories),
        "<path>  Consider given path to collect references.";

      "--underscore", Unit DeadFlag.set_underscore, " Show names starting with an underscore";

      "--verbose", Unit DeadFlag.set_verbose, " Verbose mode (ie., show scanned files)";
      "-v", Unit DeadFlag.set_verbose, " See --verbose";

      "--internal", Unit DeadFlag.set_internal,
        " Keep internal uses as exported values uses when the interface is given. \
          This is the default behaviour when only the implementation is found";

      "--nothing", Unit (update_all "nothing"), " Disable all warnings";
      "-a", Unit (update_all "nothing"), " See --nothing";
      "--all", Unit (update_all "all"), " Enable all warnings";
      "-A", Unit (update_all "all"), " See --all";

      "-E", String (DeadFlag.update_basic "-E" DeadFlag.exported),
        "<display>  Enable/Disable unused exported values warnings.\n    \
        <display> can be:\n\
          \tall\n\
          \tnothing\n\
          \t\"threshold:<integer>\": report elements used up to the given integer\n\
          \t\"calls:<integer>\": like threshold + show call sites";

      "-M", String (DeadFlag.update_basic "-M" DeadFlag.obj),
        "<display>  Enable/Disable unused methods warnings.\n    \
        See option -E for the syntax of <display>";

      "-Oa", String (DeadFlag.update_opt DeadFlag.opta),
        "<display>  Enable/Disable optional arguments always used warnings.\n    \
        <display> can be:\n\
          \tall\n\
          \tnothing\n\
          \t<threshold>\n\
          \t\"calls:<threshold>\" like <threshold> + show call sites\n    \
        <threshold> can be:\n\
          \t\"both:<integer>,<float>\": both the number max of exceptions \
          (given through the integer) and the percent of valid cases (given as a float) \
          must be respected for the element to be reported\n\
          \t\"percent:<float>\": percent of valid cases to be reported";

      "-On", String (DeadFlag.update_opt DeadFlag.optn),
        "<display>  Enable/Disable optional arguments never used warnings.\n    \
        See option -Oa for the syntax of <display>";

      "-S", String (DeadFlag.update_style),
        " Enable/Disable coding style warnings.\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
        Options (can be used together):\n\
          \tbind: useless binding\n\
          \topt: optional arg in arg\n\
          \tseq: use sequence\n\
          \tunit: unit pattern\n\
          \tall: bind & opt & seq & unit";

      "-T", String (DeadFlag.update_basic "-T" DeadFlag.typ),
        "<display>  Enable/Disable unused constructors/records fields warnings.\n    \
        See option -E for the syntax of <display>";

    ]
    (Printf.eprintf "Scanning files...\n%!";
     load_file)
    ("Usage: " ^ Sys.argv.(0) ^ " <options> <path>\nOptions are:"))


let () =
try
    parse ();
    let run_on_references_only state =
      DeadCommon.declarations := false;
      let oldstyle = !DeadFlag.style in
      DeadFlag.update_style "-all";
      List.fold_left load_file state !DeadFlag.directories
      |> ignore;
      DeadFlag.style := oldstyle
    in
    run_on_references_only (State.get_current ());

    Printf.eprintf " [DONE]\n\n%!";

    let open DeadFlag in
    !DeadLexiFi.prepare_report DeadType.decs;
    if !DeadFlag.exported.print                 then  report_unused_exported ();
    DeadObj.report();
    DeadType.report();
    if !DeadFlag.opta.DeadFlag.print || !DeadFlag.optn.DeadFlag.print
    then  begin
        let tmp = analyze_opt_args () in
        if !DeadFlag.opta.print then  report_opt_args "ALWAYS" tmp;
        if !DeadFlag.optn.print then  report_opt_args "NEVER" tmp end;
    if [@warning "-44"] DeadFlag.(!style.opt_arg || !style.unit_pat
    || !style.seq || !style.binding)            then  report_style ();

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
