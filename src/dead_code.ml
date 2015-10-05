(***************************************************************************)
(*  Copyright (C) 2014-2015 LexiFi SAS. All rights reserved.               *)
(***************************************************************************)

open Types
open Typedtree

type flags =
  {
    sub1: bool ref;
    sub2: bool ref;
    sub3: bool ref;
    sub4: bool ref;
    call: bool ref;
  }


let make_flag b = {sub1 = ref b; sub2 = ref b; sub3 = ref b; sub4 = ref b; call = ref b}

(* opt_flag = {sub1: ALWAYS; sub2: NEVER; sub3: false; sub4: false; call: show callsites *)
let opt_flag = ref @@ make_flag false

(* style_flag = {sub1: opt arg in arg; sub2: unit pattern; sub3: use sequence; sub4: useless binding; call: false *)
and style_flag = ref @@ make_flag false

(* unused_flag = {sub1: all; sub2: false; sub3: false; sub4: false; call: show callsites *)
and unused_flag = ref @@ {(make_flag true) with call=ref false}

(* exported_flag = {sub1: all; sub2: false; sub3: false; sub4: false; call: false *)
and exported_flag = ref @@ make_flag true

(* Is one of the subsections to print? *)
let status_flag flag = !(flag.sub1) || !(flag.sub2) || !(flag.sub3) || !(flag.sub4)

let verbose = ref false
let set_verbose () = verbose := true

let underscore = ref false
let set_underscore () = underscore := true

let flexibility = ref 0
let set_flexibility x = flexibility := x


let abspath = Hashtbl.create 16
let bad_files = ref []

let vds = ref []  (* all exported value declarations *)
let references = Hashtbl.create 256  (* all value references *)
let corres = Hashtbl.create 256  (* link from dec to def *)

let style = ref [] (* patterns of type unit which are not () *)
let last_loc = ref Location.none
    (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""

let unit fn = Filename.chop_extension (Filename.basename fn)

let section ?(sub = false) s =
  Printf.printf "%s %s:\n%s%s\n"
    (if sub then ">>-> " else ">>")
    s
    (if sub then "    " else "  ")
    (String.make ((if sub then 2 else 1) + String.length s + 1) (if sub then '~' else '='))

let separator () =
  Printf.printf "%s\n\n\n" (String.make 80 '-')

let prloc ?fn (loc : Location.t) = begin match fn with
  | Some s when Filename.chop_extension (loc.loc_start.pos_fname) = Filename.chop_extension (Filename.basename s) ->
      print_string (Filename.dirname s ^ "/" ^ loc.loc_start.pos_fname)
  | _ -> begin match Hashtbl.find abspath loc.loc_start.pos_fname with
    | s -> print_string s
    | exception Not_found ->
        print_string @@ Printf.sprintf "!!UNKNOWN<%s%s>!!"
          loc.loc_start.pos_fname
          (match fn with None -> "" | Some s -> " // " ^ s)
    end
  end;
  print_char ':';
  print_int loc.loc_start.pos_lnum;
  print_string ": "


type func_info =
  {
    mutable opt_args: string list;
    mutable need: int; (* Nb of mandatory arguments *)
    mutable call_sites: Location.t list;
  }

type vd_node =
  {
    loc: Location.t;
    name: string;
    func: func_info;
    mutable ptr: vd_node; (* points to itself if not a binding to another known value *)
    implem: bool;
  }

let opt_args = ref []

type opt_arg =
  {
    mutable with_val: Location.t list;
    mutable without_val: Location.t list;
  }

let vd_nodes = Hashtbl.create 256

(* Go deeper in the vd_node until cond is respected (or it is as deep as it can) *)
let rec repr ?(cond = (fun _ -> false)) n =
  if n.ptr == n || cond n then n
  else repr n.ptr

(* repr specialization to get the next node corresponding to a function *)
let next_fn_node n =
  repr ~cond:(fun n -> n.func.opt_args <> [] || n.func.need > 0) n.ptr

(* Get or create a vd_node corresponding to the location *)
let vd_node ?(name = "_unknown_") loc =
  assert (not loc.Location.loc_ghost);
  try (Hashtbl.find vd_nodes loc)
  with Not_found ->
    let fn = loc.Location.loc_start.Lexing.pos_fname in
    let implem = Filename.check_suffix fn ".mf" || Filename.check_suffix fn ".ml"  in
    let rec r = {name; loc; ptr = r; implem; func = {opt_args = []; need = 0; call_sites = []}} in
    if name <> "_unknown_" then
      Hashtbl.add vd_nodes loc r;
    r

(* Makes the most recent declaration points on the oldest one *)
let merge_nodes ~search n1 n2 =
  let n1 = search n1 and n2 = search n2 in
  if n1.implem && not n2.implem       then  n2.ptr <- n1
  else if n2.implem && not n1.implem  then  n1.ptr <- n2
  else if n1.loc < n2.loc             then  n2.ptr <- n1
  else              (*default*)             n1.ptr <- n2

(* Merge nodes in order *)
let merge_nodes_f ~search n1 n2 =
  let n1 = search n1 and n2 = search n2 in
  n1.ptr <- n2

(* Locations l1 and l2 are part of a binding from one to another *)
let merge_locs ?(force = false) ?(search = repr ~cond:(fun _ -> false)) ?name l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then
    (if force then merge_nodes_f else merge_nodes) ~search (vd_node ?name l1) (vd_node l2)


(* Verify the optional args calls. Treat args *)
let rec treat_args ?(anon = false) val_loc args =
  List.iter (check_args val_loc) args;
  if val_loc.Location.loc_ghost then () (* Ghostbuster *)
  else begin (* begin ... end for aesthetics *)
    let loc = vd_node val_loc in
    let tbl = Hashtbl.create 256 in
    let use = ref 0 in

    let treat = function
      | (Asttypes.Optional lab, expr, _) when (expr <> None || not anon) && status_flag !opt_flag->
          let has_val = match expr with
            | Some e -> begin match e.exp_desc with
              | Texp_construct(_, {cstr_name="None"; _}, _) -> false
              | _ -> true end
            | None -> anon
          in
          let occur = ref @@
            try Hashtbl.find tbl lab + 1
            with Not_found -> Hashtbl.add tbl lab 1; 1
          in
          let count x l = List.length @@ List.find_all (( = ) x) l in
          let rec locate loc =
            let count = if loc == loc.ptr then 0 else count lab loc.func.opt_args in
            if loc == loc.ptr || count >= !occur then loc
            else (occur := !occur - count; locate @@ next_fn_node loc)
          in
          if (not !underscore || lab.[0] <> '_') then
            opt_args :=
              (locate loc, lab, has_val, (match expr with
                | Some e when not e.exp_loc.Location.loc_ghost -> e.exp_loc
                | _ -> !last_loc))
              :: !opt_args
      | _ -> incr use
    in

    List.iter treat args
  end

(* Verify the nature of the argument to detect and treat function applications and uses *)
and check_args call_site = function
  | (_, None, _) -> ()
  | (_, Some e, _) ->
      let call_site =
        if call_site.Location.loc_ghost then  e.exp_loc
        else            (* default *)         call_site
      in match e.exp_desc with
        | Texp_ident (_, _, {val_loc; _}) ->
                if not val_loc.Location.loc_ghost then
              let node = vd_node val_loc in
              node.func.call_sites <- call_site :: node.func.call_sites
        | Texp_function (_,
              [{c_lhs={pat_desc=Tpat_var (_, _); pat_loc={loc_ghost=true; _}; _};
                c_rhs={exp_desc=Texp_apply (_, args); exp_loc={loc_ghost=true; _}; _}; _}], _) ->
            treat_args call_site args
        | Texp_apply ({exp_desc=Texp_ident(_, _, {val_loc; _}); _}, args) ->
            treat_args val_loc args;
            if not val_loc.Location.loc_ghost then begin
              let node = vd_node val_loc in
              node.func.call_sites <- call_site :: node.func.call_sites;
              last_loc := val_loc
            end
        | Texp_let (* Partial application as argument may cut in two parts:
                    * let _ = partial in implicit opt_args elimination *)
            ( _,
              [{vb_expr={exp_desc=Texp_apply({exp_desc=Texp_ident(_, _, {val_loc; _}); _}, _); _}; _}],
              { exp_desc=Texp_function (_,
                  [{c_lhs={pat_desc=Tpat_var (_, _); pat_loc={loc_ghost=true; _}; _};
                    c_rhs={exp_desc=Texp_apply (_, args); exp_loc={loc_ghost=true; _}; _}; _}],_);
                exp_loc={loc_ghost=true; _};_}) ->
            treat_args ~anon:true val_loc args
        | _ -> ()


(* Go down the exp to apply args on every "child". Used for conditional branching *)
let rec treat_exp exp args = match exp.exp_desc with
  | Texp_ident (_, _, {Types.val_loc; _}) ->
      treat_args ~anon:true val_loc args
  | Texp_ifthenelse (_, exp_then, exp_else) ->
      treat_exp exp_then args;
      begin match exp_else with
        | Some exp -> treat_exp exp args
        | _ -> () end
  | Texp_match (_, l1, l2, _) ->
      List.iter (fun {c_rhs=exp; _} -> treat_exp exp args) l1;
      List.iter (fun {c_rhs=exp; _} -> treat_exp exp args) l2
  | Texp_apply(exp, in_args) ->
      treat_exp exp (in_args @ args)
  | _ -> ()


(* Look for bad style typing *)
let rec check_type t loc = if !(!style_flag.sub1) then match t.desc with
  | Tlink t -> check_type t loc
  | Tarrow (lab, _, t, _) -> begin match lab with
    | Optional lab when not !underscore || lab.[0] <> '_' ->
        style := (!current_src, loc, "val f: ... -> (... -> ?_:_ -> ...) -> ...") :: !style
    | _ -> check_type t loc end
  | _ -> ()


(* Construct the 'opt_args' list of func in node *)
let rec build_node_args node expr = match expr.exp_desc with
  | Texp_function (lab, [{c_lhs={pat_desc=Tpat_var(_, _); pat_type; _}; c_rhs=exp; _}], _) ->
      check_type pat_type expr.exp_loc;
      begin match lab with
        | Asttypes.Optional s ->
            node.func.opt_args <- s::node.func.opt_args;
            build_node_args node exp
        | _ -> node.func.need <- node.func.need + 1 end
  | Texp_apply({exp_desc=Texp_ident(_, _, {val_loc=loc2; _}); _}, args) ->
      treat_args loc2 args;
      merge_locs ~force:true ~search:next_fn_node node.loc loc2
  | Texp_ident(_, _, {val_loc=loc2; _}) ->
      merge_locs ~force:true ~search:next_fn_node node.loc loc2
  | _ -> treat_exp expr @@ List.map (fun lab -> (Asttypes.Optional lab, None, Optional)) node.func.opt_args


let rec sign = function
  | Mty_signature sg -> sg
  | Mty_functor (_, _, t) -> sign t
  | Mty_ident _ | Mty_alias _ -> []

let rec collect_export path u = function
  | Sig_value (id, {Types.val_loc; _}) when not val_loc.Location.loc_ghost ->
      (* a .cmi file can contain locations from other files.
        For instance:
            module M : Set.S with type elt = int
        will create value definitions whose location is in set.mli
      *)
      if u = unit val_loc.Location.loc_start.Lexing.pos_fname
          && (not !underscore || (Ident.name id).[0] <> '_') then
        vds := (!current_src, id :: path, val_loc) :: !vds
  | Sig_module (id, {Types.md_type = t; _}, _)
  | Sig_modtype (id, {Types.mtd_type = Some t; _}) -> List.iter (collect_export (id :: path) u) (sign t)
  | _ -> ()


let is_unit t = match (Ctype.repr t).desc with
  | Tconstr (p, [], _) -> Path.same p Predef.path_unit
  | _ -> false


(* Binding in Tstr_value *)
let value_binding = function
  | {
      vb_pat={pat_desc=Tpat_var(id, {loc=loc1; _}); _};
      vb_expr={exp_desc=Texp_ident(_, _, {val_loc=loc2; _}); _};
      _
    } ->
      merge_locs ~force:true ~search:next_fn_node ~name:(Ident.name id) loc1 loc2
  | {
      vb_pat={pat_desc=Tpat_var(id, {loc=loc1; _}); _};
      vb_expr=exp;
      _
    } ->
      build_node_args (vd_node ~name:(Ident.name id) loc1) exp
  | _ -> ()


(* Parse the AST *)
let collect_references =
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
    | _ -> () end;
    super.structure_item self i
  in

  let pat self p =
    let u s = style := (!current_src, p.pat_loc, Printf.sprintf "unit pattern %s" s) :: !style in
    begin if is_unit p.pat_type && !(!style_flag.sub2) then match p.pat_desc with
      | Tpat_construct _ -> ()
      | Tpat_var (_, {txt = "eta"; loc = _}) when p.pat_loc = Location.none -> ()
      | Tpat_var (_, {txt; loc = _})-> if not !underscore || txt.[0] <> '_' then u txt
      | Tpat_any -> if not !underscore then u "_"
      | _ -> u "" end;
    super.pat self p
  in

  let expr self e = begin match e.exp_desc with
    | Texp_apply(exp, args) -> treat_exp exp args
    | Texp_ident (_, _, {Types.val_loc; _}) when not val_loc.Location.loc_ghost ->
        Hashtbl.add references val_loc (e.exp_loc :: try Hashtbl.find references e.exp_loc with Not_found -> []);
        let node = vd_node val_loc in
        node.func.call_sites <- e.exp_loc :: node.func.call_sites
    | Texp_let (_, [{vb_pat; _}], _) when is_unit vb_pat.pat_type && !(!style_flag.sub3) ->
        begin match vb_pat.pat_desc with
          | Tpat_var (id, _) when !underscore && (Ident.name id).[0] = '_' -> ()
          | _ -> style := (!current_src, vb_pat.pat_loc, "let () = ... in ... (=> use sequence)") :: !style end
    | Texp_let (
          Nonrecursive,
          [{vb_pat = {pat_desc = Tpat_var (id1, _); pat_loc; _}; _}],
          {exp_desc= Texp_ident (Pident id2, _, _); exp_extra = []; _})
      when id1 = id2 && !(!style_flag.sub4) && (not !underscore || (Ident.name id1).[0] <> '_') ->
        style := (!current_src, pat_loc, "let x = ... in x (=> useless binding)") :: !style
    | _ -> () end;
    super.expr self e
  in

  let expr = wrap expr (fun x -> x.exp_loc) in
  let pat = wrap pat (fun x -> x.pat_loc) in
  let structure_item = wrap structure_item (fun x -> x.str_loc) in
  {super with structure_item; expr; pat}


let kind fn =
  if Filename.check_suffix fn ".cmi" then
    let base = Filename.chop_suffix fn ".cmi" in
    if Sys.file_exists (base ^ ".mfi")              then  `Iface (base ^ ".mfi")
    else if Sys.file_exists (base ^ ".mli")         then  `Iface (base ^ ".mli")
    else                  (* default *)                   `Ignore
  else if Filename.check_suffix fn ".cmt" then
    let base = Filename.chop_suffix fn ".cmt" in
    if Sys.file_exists (base ^ ".mf")               then  `Implem (base ^ ".mf")
    else if Sys.file_exists (base ^ ".ml")          then  `Implem (base ^ ".ml")
    else                  (* default *)                   `Ignore
  else if (try Sys.is_directory fn with _ -> false) then  `Dir
  else                    (* default *)                   `Ignore


(* Map a local filename to an absolute path.  This currently assumes that there are never
 * two files with the same basename. *)
let regabs fn =
  current_src := fn;
  Hashtbl.add abspath (Filename.basename fn) fn

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

let rec load_file fn =
  match kind fn with
  | `Iface src ->
      (* only consider module with an explicit interface *)
      let open Cmi_format in
      last_loc := Location.none;
      if !verbose then Printf.eprintf "Scanning %s\n%!" fn;
      begin try
        regabs src;
        let u = unit fn in
        List.iter (collect_export [Ident.create (String.capitalize_ascii u)] u) (read_cmi fn).cmi_sign
      with Cmi_format.Error (Wrong_version_interface _) ->
        (*Printf.eprintf "cannot read cmi file: %s\n%!" fn;*)
        bad_files := fn :: !bad_files
      end

  | `Implem src ->
      let open Cmt_format in
      last_loc := Location.none;
      if !verbose then Printf.eprintf "Scanning %s\n%!" fn;
      regabs src;
      let cmt =
        try Some (read_cmt fn)
        with _ -> bad_files := fn :: !bad_files; None
      in
      begin match cmt with
        | None -> ()
        | Some cmt -> match cmt.cmt_annots with
            | Implementation x ->
                ignore (collect_references.structure collect_references x);
                List.iter
                  (fun (vd1, vd2) ->
                    Hashtbl.add corres vd2.Types.val_loc vd1.Types.val_loc;
                    merge_locs vd1.Types.val_loc vd2.Types.val_loc
                  )
                  cmt.cmt_value_dependencies
            | _ -> ()  (* todo: support partial_implementation? *)
      end

  | `Dir ->
      if not (is_excluded_dir fn) then
        Array.iter
          (fun s -> if s <> ".svn" then load_file (fn ^ "/" ^ s))
          (Sys.readdir fn)
      (* else Printf.eprintf "skipping directory %s\n" fn *)

  | `Ignore -> ()


let analyse_opt_args () =
  let all = ref [] in
  let tbl = Hashtbl.create 256 in

  let analyse = fun (loc, lab, has_val, callsite) ->
    let loc =
      let loc = if loc.func.opt_args = [] then next_fn_node loc else loc in
      try vd_node ~name:loc.name @@ Hashtbl.find corres loc.loc
      with Not_found -> loc
    in
    let slot =
      try Hashtbl.find tbl (loc.loc, lab)
      with Not_found ->
        let r = {with_val = []; without_val = []} in
        all := (loc.loc, lab, r) :: !all;
        Hashtbl.add tbl (loc.loc, lab) r;
        r
    in
    if has_val then slot.with_val <- callsite :: slot.with_val
    else slot.without_val <- callsite :: slot.without_val
  in

  List.iter analyse !opt_args;
  !all


(* Helpers for the following reports *)

(* Absolute path *)
let abs loc = match Hashtbl.find abspath loc.Location.loc_start.pos_fname with
  | s -> s
  | exception Not_found -> loc.Location.loc_start.pos_fname

(* Check directory change *)
let dir first =
  let prev = ref @@ Filename.dirname first (* static *)
  in fun s -> let s = Filename.dirname s in
    !prev <> s && (prev := s; true)

(* Print call site *)
let pretty_print_call () = let ghost = ref false in (* static *)function
  | loc when not loc.Location.loc_ghost ->
      print_string "         "; prloc loc;
      (fun (_, _, c) -> print_int c |> print_newline) @@ Location.get_pos_info loc.loc_start
  | _ ->
      if not !ghost then print_endline "        |~ ghost";
      ghost := true

let report s ?(extra = "Called") l nb_call pretty_print reporter =
  if l <> [] then begin
    section ~sub:(nb_call <> 0)
    @@ (if nb_call = 0 then s
        else Printf.sprintf "%s: %s %d time(s)" s extra nb_call);
        List.iter pretty_print l;
    if nb_call < !flexibility then
      print_endline "--------" |> print_newline |> print_newline
  end;
  if nb_call < !flexibility then reporter (nb_call+1)
  else if !flexibility > 0 then
    (print_newline(); print_endline "Nothing else to report in this section" |> separator)
  else if l <> [] then separator ()



let report_opt_args s l =
  let rec report_opt_args nb_call =
    let l = List.filter
        (fun (_, _, slot, ratio, _) -> ratio <> 1. && List.length slot = nb_call
        && (s = "NEVER" && !(!opt_flag.sub2) || s <> "NEVER" && !(!opt_flag.sub1)))
      @@ List.map
        (fun (loc, lab, slot) ->
          let l = if s = "NEVER" then slot.with_val else slot.without_val in
          let flen = float_of_int @@ List.length l in
          let total = List.length slot.with_val + List.length slot.without_val in
          let ratio = flen /. float_of_int total
          in (loc, lab, l, ratio, total))
        l
      |> List.fast_sort (fun (loc1, lab1, slot1, _, _) (loc2, lab2, slot2, _, _) ->
          compare (abs loc1, loc1, lab1, slot1) (abs loc2, loc2, lab2, slot2 ))
    in

    let change =
      let (loc, _, _, _, _) = try List.hd l with _ -> (!last_loc, "_none_", [], 0., 0) in
      dir @@ abs loc
    in

    let pretty_print = fun (loc, lab, slot, ratio, total) ->
      if change @@ abs loc then print_newline ();
      prloc loc; print_string lab;
      if ratio <> 0. then begin
        print_string " ("; print_float (100. -. 100. *. ratio); Printf.printf "%% out of %d call(s))" total;
        if !(!opt_flag.call) then print_string "  Exceptions:"
      end;
      print_newline ();
      if !(!opt_flag.call) && ratio <> 1. then begin
        List.iter (pretty_print_call ()) slot;
        if nb_call <> 0 then print_newline()
      end
    in

    let s =
      (if nb_call > 0 then "OPTIONAL ARGUMENTS: ALMOST "
      else "OPTIONAL ARGUMENTS: ") ^ s
    in
    report s ~extra:"Except" l nb_call pretty_print report_opt_args;

  in report_opt_args 0


let report_unused_exported () =
  let rec report_unused_exported nb_call =
    let l =
      List.fold_left
        (fun acc (fn, path, loc) ->
          let l = ref [] in
          match not (Hashtbl.mem references loc
                    && List.length @@ (l := Hashtbl.find references loc; !l) <> nb_call
                  || (let loc = Hashtbl.find corres loc in Hashtbl.mem references loc
                    && List.length @@ (l := Hashtbl.find references loc; !l) <> nb_call))
                && List.length !l = nb_call with
            | exception Not_found when nb_call = 0 -> (fn, path, loc, !l)::acc
            | exception Not_found -> acc
            | true -> (fn, path, loc, !l)::acc
            | false -> acc)
        [] !vds
      |> List.fast_sort (fun (fn1, path1, loc1, _) (fn2, path2, loc2, _) ->
          compare (fn1, abs loc1, path1) (fn2, abs loc2, path2))
    in

    let change =
      let (fn, _, _, _) = try List.hd l with _ -> ("_none_", [], !last_loc, []) in
      dir fn
    in
    let pretty_print = fun (fn, path, loc, call_sites) ->
      if change fn then print_newline ();
      prloc ~fn loc;
      print_endline (String.concat "." @@ List.tl @@ (List.rev_map Ident.name path));
        if !(!exported_flag.call) then begin
          List.iter (pretty_print_call ()) call_sites;
          if nb_call <> 0 then print_newline()
        end
    in

    let s = if nb_call = 0 then "UNUSED EXPORTED VALUES" else "ALMOST UNUSED EXPORTED VALUES" in
    report s l nb_call pretty_print report_unused_exported

  in report_unused_exported 0


let report_unused () =
  let rec report_unused nb_call =
    let exportable node =
      List.exists
        (fun (fn, path, _) ->
            unit fn = unit node.loc.loc_start.pos_fname
            && Ident.name @@ List.hd path = node.name)
        !vds
    in
    let l =
      Hashtbl.fold
        (fun _ node l ->
            if List.length node.func.call_sites = nb_call && not (exportable node)
                && (not !underscore || node.name.[0] <> '_') then node::l
            else l)
        vd_nodes []
      |> List.fast_sort (fun n1 n2 ->
          let cmp = compare (abs n1.loc) (abs n2.loc) in
          if cmp = 0 then compare n1 n2 else cmp)
    in

    let change = dir @@ (try abs (List.hd l).loc with _ -> "_none_") in
    let pretty_print =fun node ->
      if change @@ abs node.loc then print_newline ();
      prloc node.loc; print_string node.name;
        print_newline ();
        if !(!unused_flag.call) then begin
          List.iter (pretty_print_call ()) node.func.call_sites;
          if nb_call <> 0 then print_newline()
        end
    in

    let s = if nb_call > 0 then "ALMOST UNUSED VALUES" else "UNUSED VALUES" in
    report s l nb_call pretty_print report_unused

  in report_unused 0


let report_style () =
  if !style <> [] then begin
    section "CODING STYLE";
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
    separator ()
  end


(* Init *)
let set_all fn =
  opt_flag := {!opt_flag with sub3 = ref false; sub4 = ref false};
  exported_flag := {!exported_flag with sub2 = ref false; sub3 = ref false; sub4 = ref false};
  unused_flag := {!unused_flag with sub2 = ref false; sub3 = ref false; sub4 = ref false};
  Printf.eprintf "Scanning files...\n%!";
  load_file fn

let parse () =
  let update_all b () =
    style_flag := {(make_flag b) with call = !style_flag.call};
      exported_flag := {(make_flag b) with call = !exported_flag.call};
      unused_flag := {(make_flag b) with call = !unused_flag.call};
      opt_flag := {(make_flag b) with call = !opt_flag.call}
  in

  let update_flag b flag s =
    let updt = (* to avoid to much repetition in code *)
      [ (fun () -> !flag.sub1 := b);
        (fun () -> !flag.sub2 := b);
        (fun () -> !flag.sub3 := b);
        (fun () -> !flag.sub4 := b)]
    in
    let rec aux l =
      match l with
        | ("1" as e)::l | ("2" as e)::l | ("3" as e)::l | ("4" as e)::l ->
            List.nth updt (int_of_string e) @@ (); aux l
        | "all"::l -> flag := {(make_flag b) with call = !flag.call};
            aux l
        | "call-site"::l -> !flag.call := b;
            aux l
        | ""::l -> aux l
        | s::_ -> raise @@ Arg.Bad ("unknwon option: " ^ s)
        | [] -> ()
    in aux @@ Str.split (Str.regexp "\\+") s
  in

  Arg.(parse
    [ "--exclude-directory", String exclude_dir, "<directory>  Exclude given directory from research.";

      "--no-underscore", Unit set_underscore, " Hide names starting with an underscore";

      "--verbose", Unit set_verbose, " Verbose mode (ie., show scanned files)";
      "-v", Unit set_verbose, " Verbose mode (ie., show scanned files)";

      "--flexibility", Int set_flexibility, "<integer> Number max of exceptions to still be considered in the category";

      "-a", Unit (update_all false), " Disable all warnings";
      "--nothing", Unit (update_all false), " Disable all warnings";
      "-A", Unit (update_all true), " Enable all warnings";
      "--all", Unit (update_all true), " Enable all warnings";

      "-e", Unit (fun () -> !exported_flag.sub1 := false), " Disable unused exported values warnings";
      "-E", String (update_flag true exported_flag),
        " Enable unused exported values warnings. Options (can be used together):\n\
          \t+all: Enable warnings\n\
          \t+call-site: show call sites";

      "-o", String (update_flag false opt_flag),
        " Disable optional arguments warnings. Options:\n\
          \tSee -O";
      "-O", String (update_flag true opt_flag),
        " Enable optional arguments warnings. Options (can be used together):\n\
          \t+1: ALWAYS\n\
          \t+2: NEVER\n\
          \t+all: 1+2\n\
          \t+call-site: show call sites";

      "-s", String (update_flag false style_flag),
        " Disable coding style warnings. Options:\n\
          \tSee -S";
      "-S", String (update_flag true style_flag),
        " Enable coding style warnings. Options (can be used together):\n\
          \t+1: optional arg in arg\n\
          \t+2: unit pattern\n\
          \t+3: use sequence\n\
          \t+4: useless binding\n\
          \t+all: 1+2+3+4";

      "-u", Unit (fun () -> !unused_flag.sub1 := false), " Disable unused values warnings";
      "-U", String (update_flag true unused_flag),
        " Enable unused values warnings. Options (can be used together):\n\
          \t+all: Enable warnings\n\
          \t+call-site: show call sites";
    ]
    set_all
    ("Usage: " ^ Sys.argv.(0) ^ " <options> <directory|file>\nOptions are:"))


let () =
  try
    parse ();
    Printf.eprintf " [DONE]\n\n%!";

    if (status_flag !unused_flag)    then report_unused ();
    if (status_flag !exported_flag)  then report_unused_exported ();
    if (status_flag !opt_flag)       then begin let tmp = analyse_opt_args () in
                                          report_opt_args "ALWAYS" tmp;
                                          report_opt_args "NEVER" tmp end;
    if (status_flag !style_flag)     then report_style ();

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
