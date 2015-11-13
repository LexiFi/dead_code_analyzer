(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Dead code anlyzing tool. It only reports unused exported values by default.
 * Options can enable reporting of optional arguments always/never used as bad style of code.
 * In addition to selecting which reports are to be displayed, the limit of authorized
 * occurences needed to be reported can be selected (default is 0).
 * It assumes .mli are compiled with -keep-locs and .ml are compiled with -bin-annot.
 *)

open Types
open Typedtree

open DeadCommon


                (********   ATTRIBUTES   ********)

let bad_files = ref []

let var_name = ref ""


                (********   PROCESSING   ********)

(* Go down the exp to apply args on every "child". Used for conditional branching *)
let rec treat_exp exp args = match exp.exp_desc with
  | Texp_ident (_, _, {Types.val_loc; _})
  | Texp_field (_, _, {lbl_loc=val_loc; _}) ->
      DeadArg.process val_loc args
  | Texp_ifthenelse (_, exp_then, exp_else) ->
      treat_exp exp_then args;
      begin match exp_else with
        | Some exp -> treat_exp exp args
        | _ -> () end
  | Texp_match (_, l1, l2, _) ->
      List.iter (fun {c_rhs=exp; _} -> treat_exp exp args) l1;
      List.iter (fun {c_rhs=exp; _} -> treat_exp exp args) l2
  | Texp_apply (exp, in_args) ->
      treat_exp exp (in_args @ args)
  | _ -> ()


(* Binding in Tstr_value *)
let value_binding = function
  | {
      vb_pat={pat_desc=Tpat_var (_, {loc=loc1; _}); _};
      vb_expr={exp_desc=Texp_ident (_, _, {val_loc=loc2; _}); _};
      _
    } ->
      merge_locs ~add:true loc1 loc2
  | {
      vb_pat={pat_desc=Tpat_var (_, {loc=loc1; _}); _};
      vb_expr=exp;
      _
    } when not loc1.loc_ghost ->
      DeadArg.node_build (vd_node ~add:true loc1) exp
  | _ -> ()


(* Parse the AST *)
let collect_references =                          (* Tast_mapper *)
  let super = Tast_mapper.default in
  let wrap f loc self x =
    let l = !last_loc in
    let ll = loc x in
    if ll <> Location.none then last_loc := ll;
    let r = f self x in
    last_loc := l;
    r
  in

  let structure_item self i = begin match i.str_desc with
    | Tstr_value (_, l) -> List.iter value_binding l
    | Tstr_type  (_, l) when !DeadFlag.exported.print -> List.iter DeadType.tstr l
    | Tstr_module  {mb_name={txt; _};_} -> mods := txt :: !mods
    | Tstr_class l -> List.iter DeadObj.tstr l
    | _ -> () end;
    let res = super.structure_item self i in
    begin match i.str_desc with
    | Tstr_module _ -> mods := List.tl !mods
    | _ -> () end;
    res
  in

  let pat self p =
    let u s = style := (!current_src, p.pat_loc, Printf.sprintf "unit pattern %s" s) :: !style in
    begin if DeadType.is_unit p.pat_type && !DeadFlag.style.unit_pat then match p.pat_desc with (* look for unit pattern *)
      | Tpat_construct _ -> ()
      | Tpat_var (_, {txt = "eta"; loc = _}) when p.pat_loc = Location.none -> ()
      | Tpat_var (_, {txt; _})-> if check_underscore txt then u txt
      | Tpat_any -> if not !DeadFlag.underscore then u "_"
      | _ -> u "" end;
    begin match p.pat_desc with
      | Tpat_record (l, _) ->
          List.iter
            (fun (_, lab, _) ->
              if exported lab.lbl_loc.Location.loc_start.pos_fname then
                Hashtbl.replace references lab.lbl_loc (p.pat_loc :: hashtbl_find_list references lab.lbl_loc))
            l
      | Tpat_var (id, _) -> var_name := id.Ident.name
      | _ -> () end;
    super.pat self p
  in

  let expr self e = begin match e.exp_desc with   (* most of the processing starts here *)
    | Texp_apply (exp, args) ->
        if DeadFlag.(!opt.always || !opt.never) then treat_exp exp args;
        begin match exp.exp_desc with
          | Texp_ident (_, _, {Types.val_loc; _})
          when val_loc.Location.loc_ghost -> (* The application is due to lookup preparation
              * anticipated in the typedtree, wich is a case we do not want to treat
              * otherwise all object's content would be marked as used at this point... *)
              ()
          | Texp_ident (_, _, {val_type; _}) ->
              DeadObj.arg val_type args
          | _ ->
              DeadObj.arg exp.exp_type args end

    | Texp_ident (_, _, {Types.val_loc=loc; _})
    | Texp_field (_, _, {lbl_loc=loc; _})
    | Texp_construct (_, {cstr_loc=loc; _}, _)
      when not loc.Location.loc_ghost && exported loc.Location.loc_start.pos_fname ->
        Hashtbl.replace references loc (e.exp_loc :: hashtbl_find_list references loc)

    | Texp_send _ ->
          DeadObj.collect_references e

    | Texp_override (_, _) -> DeadObj.aliases := (!var_name, !DeadObj.last_class) :: !DeadObj.aliases

    | Texp_let (_, [{vb_pat; _}], _) when DeadType.is_unit vb_pat.pat_type && !DeadFlag.style.seq ->
        begin match vb_pat.pat_desc with
          | Tpat_var (id, _) when not (check_underscore (Ident.name id)) -> ()
          | _ -> style := (!current_src, vb_pat.pat_loc, "let () = ... in ... (=> use sequence)") :: !style end

    | Texp_let (
          Nonrecursive,
          [{vb_pat = {pat_desc = Tpat_var (id1, _); pat_loc; _}; _}],
          {exp_desc= Texp_ident (Pident id2, _, _); exp_extra = []; _})
      when id1 = id2 && !DeadFlag.style.binding && check_underscore (Ident.name id1) ->
        style := (!current_src, pat_loc, "let x = ... in x (=> useless binding)") :: !style

    | _ -> () end;
    super.expr self e
  in

  let expr = wrap expr (fun x -> x.exp_loc) in
  let pat = wrap pat (fun x -> x.pat_loc) in
  let structure_item = wrap structure_item (fun x -> x.str_loc) in
  let class_structure = (fun self x -> DeadObj.class_structure x; super.class_structure self x) in
  let class_field = (fun self x -> DeadObj.class_field x; super.class_field self x) in
  let class_field = wrap class_field (fun x -> x.cf_loc) in
  {super with structure_item; expr; pat; class_structure; class_field}


let rec collect_export path u signature =

  let rec sign = function
    | Mty_signature sg -> sg
    | Mty_functor (_, _, t) -> sign t
    | Mty_ident _ | Mty_alias _ -> []
  in

  match signature with
    | Sig_value (id, {Types.val_loc; Types.val_type; _}) when not val_loc.Location.loc_ghost ->
        (* a .cmi file can contain locations from other files.
          For instance:
              module M : Set.S with type elt = int
          will create value definitions whose location is in set.mli
        *)
        if not (List.fold_left (fun b str -> b || DeadType.match_str val_type str) false !DeadFlag.types) then
          export path u id val_loc;
    | Sig_type (id, t, _) ->
          DeadType.collect_export (id::path) u t
    | Sig_class (id, {Types.cty_type = t; cty_loc = loc; _}, _) -> DeadObj.collect_export (id::path) u t loc
    | Sig_module (id, {Types.md_type = t; _}, _)
    | Sig_modtype (id, {Types.mtd_type = Some t; _}) -> List.iter (collect_export (id :: path) u) (sign t)
    | _ -> ()


(* Checks the nature of the file *)
let kind fn =
  if Filename.check_suffix fn ".cmi" then
    let base = Filename.chop_suffix fn ".cmi" in
    if      Sys.file_exists (base ^ ".mli")         then  `Iface (base ^ ".mli")
    else if Sys.file_exists (base ^ ".ml")          then  `Iface (base ^ ".ml")
    else                  (* default *)                   `Ignore
  else if Filename.check_suffix fn ".cmt" then
    let base = Filename.chop_suffix fn ".cmt" in
    if      Sys.file_exists (base ^ ".ml")          then  `Implem (base ^ ".ml")
    else                  (* default *)                   `Ignore
  else if (try Sys.is_directory fn with _ -> false) then  `Dir
  else                    (* default *)                   `Ignore


(* Map a local filename to an absolute path. This currently assumes that there are never
 * two files with the same basename. *)
let regabs fn =
  current_src := fn;
  abspath := fn :: !abspath


(* Useful when the `--exclude-directory' option is used *)
let exclude_dir, is_excluded_dir =
  let tbl = Hashtbl.create 10 in
  let rec split_path s =
    let open Filename in
    if s = current_dir_name then [s]
    else (basename s) :: (split_path (dirname s))
  in
  let rec norm_path = function
    | [] -> []
    | x :: ((y :: _) as yss) when x = y && x = Filename.current_dir_name -> norm_path yss
    | x :: xss ->
        let yss = List.filter (fun x -> x <> Filename.current_dir_name) xss in
        x :: yss
  in
  let rec concat_path = function
    | [] -> ""
    | x :: xs -> Filename.concat x (concat_path xs)
  in
  let normalize_path s = concat_path (norm_path (List.rev (split_path s))) in
  let exclude_dir s = Hashtbl.replace tbl (normalize_path s) () in
  let is_excluded_dir s = Hashtbl.mem tbl (normalize_path s) in
  exclude_dir, is_excluded_dir


let read_interface fn src = let open Cmi_format in
  try
    regabs src;
    let u = unit fn in
    if !DeadFlag.exported.print then
      List.iter (collect_export [Ident.create (String.capitalize_ascii u)] u) (read_cmi fn).cmi_sign
  with Cmi_format.Error (Wrong_version_interface _) ->
    (*Printf.eprintf "cannot read cmi file: %s\n%!" fn;*)
    bad_files := fn :: !bad_files


(* Starting point *)
let rec load_file fn = match kind fn with
  | `Iface src ->
      (* only consider module with an explicit interface *)
      last_loc := Location.none;
      if !DeadFlag.verbose then Printf.eprintf "Scanning %s\n%!" fn;
      read_interface fn src

  | `Implem src ->
      let open Cmt_format in
      last_loc := Location.none;
      if !DeadFlag.verbose then Printf.eprintf "Scanning %s\n%!" fn;
      regabs src;
      let cmt =
        try Some (read_cmt fn)
        with _ -> bad_files := fn :: !bad_files; None
      in

      (* Used if the cmt is valid. Associates the two value|type dependencies *)
      let assoc (vd1, vd2) =
        let fn1 = vd1.Location.loc_start.pos_fname and fn2 = vd2.Location.loc_start.pos_fname in
        let is_implem fn = Filename.check_suffix fn ".ml" in
        let has_iface fn =
          fn <> "_none_" && (fn.[String.length fn - 1] = 'i'
            ||  try Sys.file_exists (find_path fn ^ "i")
                with Not_found -> false)
        in
        if (!DeadFlag.internal || fn1 <> fn2) && is_implem fn1 && is_implem fn2 then
          Hashtbl.replace references vd1 (vd2 ::hashtbl_find_list references vd1)
        else if not (is_implem fn1 && has_iface fn1) then begin
          Hashtbl.replace corres vd1 (vd2 :: hashtbl_find_list corres vd1);
          Hashtbl.replace references vd1 @@ List.sort_uniq compare
            ((hashtbl_find_list references vd1)
            @ hashtbl_find_list references vd2)
        end
        else begin
          Hashtbl.replace corres vd2 (vd1 :: hashtbl_find_list corres vd2);
          Hashtbl.replace references vd2 @@ List.sort_uniq compare
            ((hashtbl_find_list references vd2)
            @ hashtbl_find_list references vd1)
        end
      in

      begin match cmt with
        | Some {cmt_annots=Implementation x; cmt_value_dependencies; _} ->
            ignore (collect_references.structure collect_references x);
            if !DeadFlag.exported.print then begin
              List.iter assoc (List.rev_map (fun (vd1, vd2) -> (vd1.Types.val_loc, vd2.Types.val_loc)) cmt_value_dependencies);
              List.iter assoc !DeadType.dependencies
            end;
            DeadType.dependencies := [];
            DeadObj.defined := []
        | _ -> ()  (* todo: support partial_implementation? *)
      end

  | `Dir when not (is_excluded_dir fn) ->
      let next = Sys.readdir fn in
      Array.sort compare next;
      Array.iter
        (fun s -> load_file (fn ^ "/" ^ s))
        next
      (* else Printf.eprintf "skipping directory %s\n" fn *)

  | _ -> ()


                (********   REPORTING   ********)

(* Prepare the list of opt_args for report *)
let analyze_opt_args () =
  let all = ref [] in
  let tbl = Hashtbl.create 256 in

  let analyze = fun (loc, lab, has_val, callsite) ->
    let slot =
      try Hashtbl.find tbl (loc, lab)
      with Not_found ->
        let r = {with_val = []; without_val = []} in
        all := (loc, lab, r) :: !all;
        Hashtbl.add tbl (loc, lab) r;
        r
    in
    if has_val then slot.with_val <- callsite :: slot.with_val
    else slot.without_val <- callsite :: slot.without_val
  in

  List.iter analyze !opt_args;
  List.iter                   (* Remove call sites accounted more than once for the same element *)
    (fun (_, _, slot) ->
      slot.with_val     <- List.sort_uniq compare slot.with_val;
      slot.without_val  <- List.sort_uniq compare slot.without_val)
    !all;
  !all


        (**** Helpers for the following reports ****)

(* Absolute path *)
let abs loc = match find_path loc.Location.loc_start.pos_fname with
  | s -> s
  | exception Not_found -> loc.Location.loc_start.pos_fname


(* Check directory change *)
let dir first =
  let prev = ref @@ Filename.dirname first
  in fun s -> let s = Filename.dirname s in
    !prev <> s && (prev := s; true)


(* Faster than 'List.length l = len' when len < List.length l; same speed otherwise*)
let rec check_length len = function
  | [] -> len = 0
  | _::l when len > 0 -> check_length (len - 1) l
  | _ -> false


(* Print call site *)
let pretty_print_call () = let ghost = ref false in function
  | {Location.loc_ghost=true; _} when !ghost -> ()
  | loc when not loc.Location.loc_ghost ->
      print_string "         "; prloc loc |> print_newline
  | _ ->          (* first ghost met *)
      print_endline "        |~ ghost";
      ghost := true


let percent base = 1. -. (float_of_int base) *. (1. -. !DeadFlag.threshold.percentage) /. 10.


(* Base pattern for reports *)
let report s ?(extra = "Called") l continue nb_call pretty_print reporter =
  if nb_call = 0 || l <> [] then begin
    section ~sub:(nb_call <> 0)
    @@ (if nb_call = 0 then s
        else if !DeadFlag.threshold.optional = `Both || extra = "Called" then
          Printf.sprintf "%s: %s %d time(s)" s extra nb_call
        else Printf.sprintf "%s: at least %3.2f%% of the time" s (100. *. percent nb_call));
    List.iter pretty_print l;
    if continue nb_call then
      (if l <> [] then print_endline "--------" else ()) |> print_newline |> print_newline
  end;
  if continue nb_call then reporter (nb_call + 1)
  else (print_newline () |> separator)


let report_opt_args s l =
  let rec report_opt_args nb_call =
    let l = List.filter
        (fun (_, _, slot, ratio, _) -> let ratio = 1. -. ratio in
          if !DeadFlag.threshold.optional = `Both then
            ratio >= !DeadFlag.threshold.percentage && check_length nb_call slot
          else ratio >= percent nb_call
            && (!DeadFlag.threshold.percentage >= 1. || ratio < (percent (nb_call - 1))))
      @@ List.map
        (fun (loc, lab, slot) ->
          let l = if s = "NEVER" then slot.with_val else slot.without_val in
          let total = List.length slot.with_val + List.length slot.without_val in
          let ratio = float_of_int (List.length l) /. float_of_int total
          in (loc, lab, l, ratio, total))
        l
      |> List.fast_sort (fun (loc1, lab1, slot1, _, _) (loc2, lab2, slot2, _, _) ->
          compare (abs loc1, loc1, lab1, slot1) (abs loc2, loc2, lab2, slot2))
    in

    let change =
      let (loc, _, _, _, _) = try List.hd l with _ -> (!last_loc, "_none_", [], 0., 0) in
      dir (abs loc)
    in

    let pretty_print = fun (loc, lab, slot, ratio, total) ->
      if change (abs loc) then print_newline ();
      prloc loc; print_string ("?" ^ lab);
      if ratio <> 0. then begin
        Printf.printf "   (%d/%d calls)" (total - List.length slot) total;
        if !DeadFlag.opt.call_sites then print_string "  Exceptions:"
      end;
      print_newline ();
      if !DeadFlag.opt.call_sites then begin
        List.iter (pretty_print_call ()) slot;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call =
      !DeadFlag.threshold.optional = `Both && nb_call < !DeadFlag.threshold.exceptions
      || !DeadFlag.threshold.optional = `Percent && percent nb_call > !DeadFlag.threshold.percentage
    in
    let s =
      (if nb_call > 0 then "OPTIONAL ARGUMENTS: ALMOST "
      else "OPTIONAL ARGUMENTS: ") ^ s
    in
    report s ~extra:"Except" l continue nb_call pretty_print report_opt_args;

  in report_opt_args 0


let report_unused_exported () =
  DeadObj.prepare_report ();
  let rec report_unused_exported nb_call =
    let l =
      let folder = fun acc (fn, path, loc) ->
        let rec cut_main s pos =
          if pos = String.length s then s
          else if s.[pos] = '.' then String.sub s (pos + 1) (String.length s - pos - 1)
          else cut_main s (pos + 1)
        in
        let l = ref [] in
        let test tbl elt =
          Hashtbl.mem tbl elt
          && not (check_length nb_call (l := Hashtbl.find tbl elt; !l))
        in
        match
          ( if String.contains path '#' then not (test DeadObj.references path)
            else not (test references loc || test DeadObj.references path
            || ( let loc = Hashtbl.find corres loc in
              List.fold_left (fun res node -> res || test references node) false loc)))
        && check_length nb_call !l with
          | exception Not_found when nb_call = 0 -> (fn, cut_main path 0, loc, !l)::acc
          | exception Not_found -> acc
          | true -> (fn, cut_main path 0, loc, !l)::acc
          | false -> acc
      in
      List.fold_left folder [] !decs
      |> List.fast_sort (fun (fn1, path1, loc1, _) (fn2, path2, loc2, _) ->
          compare (fn1, loc1, path1) (fn2, loc2, path2))
    in

    let change =
      let (fn, _, _, _) = try List.hd l with _ -> ("_none_", "", !last_loc, []) in
      dir fn
    in
    let pretty_print = fun (fn, path, loc, call_sites) ->
      if change fn then print_newline ();
      prloc ~fn loc;
      print_string path;
      if call_sites <> [] && !DeadFlag.exported.call_sites then print_string "    Call sites:";
      print_newline ();
      if !DeadFlag.exported.call_sites then begin
        List.iter (pretty_print_call ()) call_sites;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call = nb_call < !DeadFlag.threshold.exceptions in
    let s = if nb_call = 0 then "UNUSED EXPORTED VALUES" else "ALMOST UNUSED EXPORTED VALUES" in
    report s l continue nb_call pretty_print report_unused_exported

  in report_unused_exported 0


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
  let update_all b () =
    DeadFlag.update_style (b ^ "all");
    DeadFlag.update_exported (b ^ "all");
    DeadFlag.update_opt (b ^ "all")
  in

  (* any extra argument can be accepted by any option using some
   * although it doesn't necessary affects the results (e.g. -O 3+4) *)
  Arg.(parse
    [ "--exclude-directory", String exclude_dir, "<directory>  Exclude given directory from research.";

      "--no-underscore", Unit DeadFlag.set_underscore, " Hide names starting with an underscore";

      "--verbose", Unit DeadFlag.set_verbose, " Verbose mode (ie., show scanned files)";
      "-v", Unit DeadFlag.set_verbose, " See --verbose";

      "--threshold", String DeadFlag.update_threshold,
        " Report values that are almost in a category.\n    \
          Delimiters '+' and '-' can both be used.\n    \
          Options (can be used together):\n\
          \t<integer>: Maximum number of exceptions. Default is 0.\n\
          \t<float>: Minimum percentage (between 0.0 and 1.0) of valid cases (for optional arguments). Default is 1.0.\n\
          \tpercent: Optional arguments have to respect the percentage only. Default behaviour\n\
          \tboth: Optional arguments have to respect both constraints";

      "--types", String DeadFlag.update_types,
        "<type list>  Ignore values of specified types.\n    \
          Delimiters '+' and '-' can both be used.\n    \
          <type list> types, starting with a delimiter:\n\
          \t<type>: The type as it would be printed in the toplevel. (e.g: 'a * int -> bool list)";

      "--call-sites", String DeadFlag.update_call_sites,
        " Reports call sites for exceptions in the given category (only useful when used with the threshold option).\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
          Options (can be used together):\n\
          \tE: Equivalent to -E +calls.\n\
          \tO: Equivalent to -O +calls.\n\
          \tall: O & E";

      "--internal", Unit DeadFlag.set_internal, " Keep internal uses as exported values uses";

      "--nothing", Unit (update_all "-"), " Disable all warnings";
      "-a", Unit (update_all "-"), " See --nothing";
      "--all", Unit (update_all "+"), " Enable all warnings";
      "-A", Unit (update_all "+"), " See --all";

      "-E", String (DeadFlag.update_exported),
        " Enable/Disable unused exported values warnings.\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
        Options (can be used together):\n\
          \tall\n\
          \tcalls: show call sites";

      "-O", String (DeadFlag.update_opt),
        " Enable/Disable optional arguments warnings.\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
        Options (can be used together):\n\
          \talways\n\
          \tnever\n\
          \tall: always & never\n\
          \tcalls: show call sites";

      "-S", String (DeadFlag.update_style),
        " Enable/Disable coding style warnings.\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
        Options (can be used together):\n\
          \tbind: useless binding\n\
          \topt: optional arg in arg\n\
          \tseq: use sequence\n\
          \tunit: unit pattern\n\
          \tall: bind & opt & seq & unit";
    ]
    (Printf.eprintf "Scanning files...\n%!";
    load_file)
    ("Usage: " ^ Sys.argv.(0) ^ " <options> <directory|file>\nOptions are:"))


let () =
  try
    parse ();
    Printf.eprintf " [DONE]\n\n%!";

    if !DeadFlag.exported.print                 then  report_unused_exported ();
    if DeadFlag.(!opt.always || !opt.never)     then  begin let tmp = analyze_opt_args () in
                if !DeadFlag.opt.always         then  report_opt_args "ALWAYS" tmp;
                if !DeadFlag.opt.never          then  report_opt_args "NEVER" tmp end;
    if !DeadFlag.style.opt_arg || !DeadFlag.style.unit_pat
    || !DeadFlag.style.seq || !DeadFlag.style.binding            then  report_style ();

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
