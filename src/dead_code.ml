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
  }


let make_flag b = {sub1 = ref b; sub2 = ref b; sub3 = ref b; sub4 = ref b}

(*opt_flag = {sub1: ALWAYS; sub2: NEVER; sub3: false; sub4: false *)
let opt_flag = ref @@ make_flag false
(*style_flag = {sub1: opt arg in arg; sub2: unit pattern; sub3: use sequence; sub4: useless binding *)
and style_flag = ref @@ make_flag false
(*unused_flag = {sub1: all; sub2: false; sub3: false; sub4: false; *)
and unused_flag = ref @@ make_flag true
(*exported_flag = {sub1: all; sub2: false; sub3: false; sub4: false; *)
and exported_flag = ref @@ make_flag true

let status_flag flag = !(flag.sub1) || !(flag.sub2) || !(flag.sub3) || !(flag.sub4)


let abspath = Hashtbl.create 16
let bad_files = ref []

let vds = ref []  (* all exported value declarations *)
let references = Hashtbl.create 256  (* all value references *)
let corres = Hashtbl.create 256  (* link from dec to def *)

let style = ref [] (* patterns of type unit which are not () *)
let last_loc = ref Location.none
    (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""

let verbose = ref false
let set_verbose () = verbose := true

let unit fn = Filename.chop_extension (Filename.basename fn)

let section s =
  Printf.printf "%s:\n%s\n" s (String.make (String.length s + 1) '=')

let separator () =
  Printf.printf "%s\n\n\n" (String.make 80 '-')

let prloc ?fn (loc : Location.t) =
  begin match fn with
  | Some s when Filename.chop_extension (loc.loc_start.pos_fname) = Filename.chop_extension (Filename.basename s) ->
      print_string (Filename.dirname s ^ "/" ^ loc.loc_start.pos_fname)
  | _ ->
      begin match Hashtbl.find abspath loc.loc_start.pos_fname with
      | s -> print_string s
      | exception Not_found -> print_string (Printf.sprintf "!!UNKNOWN<%s%s>!!" loc.loc_start.pos_fname (match fn with None -> "" | Some s -> " // " ^ s))
      end
  end;
  print_char ':';
  print_int loc.loc_start.pos_lnum;
  print_string ": "


type func_info =
  {
    mutable opt_args: string list;
    mutable need: int; (* Nb of mandatory arguments *)
    mutable used: bool; (* Complete application or passed as argument or binded *)
  }

type vd_node =
  {
    name: string;
    loc: Location.t;
    mutable ptr: vd_node; (* points to itself if not a binding to another known value *)
    implem: bool;
    func: func_info;
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
  else (if n.func.need = 0 then
      n.func.used <- true;
    repr n.ptr)

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
    let rec r = {name; loc; ptr = r; implem; func = {opt_args = []; need = 0; used = false}} in
    Hashtbl.add vd_nodes loc r;
    r

(* Makes the most recent declaration points on the oldest one *)
let merge_nodes ~search n1 n2 =
  let n1 = search n1 and n2 = search n2 in
  if n1.implem && not n2.implem then n2.ptr <- n1
  else if n2.implem && not n1.implem then n1.ptr <- n2
  else if n1.loc < n2.loc then n2.ptr <- n1 else n1.ptr <- n2;
  n1.func.used <- true;
  n2.func.used <- true

(* Merge nodes in order *)
let merge_nodes_f ~search n1 n2 =
  let n1 = search n1 and n2 = search n2 in
  if n2.func.need = 0 then n2.func.used <- true;
  n1.ptr <- n2

(* Locations l1 and l2 are part of a binding from one to another *)
let merge_locs ?(force = false) ?(search = repr ~cond:(fun _ -> false)) ?name l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then
    (if force then merge_nodes_f else merge_nodes) ~search (vd_node ?name l1) (vd_node l2)


let rec treat_args ?(anon = false) val_loc args =
  check_args args;
  if val_loc.Location.loc_ghost then () (* Ghostbuster *)
  else begin
    let loc = vd_node val_loc in
    let tbl = Hashtbl.create 256 in
    let use = ref 0 in
    List.iter
      (function
        | (Asttypes.Optional lab, (None as expr), _)
        | (Asttypes.Optional lab, (Some _ as expr), _)
          when (expr <> None || not anon) && status_flag !opt_flag->
            let has_val = match expr with
              | Some e -> begin match e.exp_desc with
                | Texp_construct(_,{cstr_name="None";_},_) -> false
                | _ -> true end
              | None -> false
            in
            let occur = ref (
              try Hashtbl.find tbl lab + 1
              with Not_found -> Hashtbl.add tbl lab 1; 1)
            in
            let count x l = List.length @@ List.find_all (( =) x) l in
            let rec locate loc =
              let count = if loc == loc.ptr then 0 else count lab loc.func.opt_args in
              if loc == loc.ptr || count >= !occur then loc
              else (occur := !occur - count; locate @@ next_fn_node loc)
            in
            opt_args :=
              (locate loc, lab, has_val, (match expr with Some e -> e.exp_loc | _ -> !last_loc))
              :: !opt_args
        | (_, Some _, _)  ->
            incr use
        | _ -> incr use
      )
      args;
    if !use >= loc.func.need then loc.func.used <- true
  end

and check_args args=
  List.iter
    (function
      | (_, Some e, _) -> begin match e.exp_desc with
        | Texp_apply ({exp_desc=Texp_ident(_, _, {val_loc; _}); _}, args) ->
            treat_args val_loc args;
            if not val_loc.Location.loc_ghost then (
              (vd_node val_loc).func.used <- true;
              last_loc := val_loc;)
        | Texp_ident (_, _, {val_loc; _}) ->
            if not val_loc.Location.loc_ghost then
              (vd_node val_loc).func.used <- true
        | Texp_let (* Partial application as argument may cut in two parts:
                    * let _ = partial in implicit opt_args elimination *)
              ( _,
                [{vb_expr={exp_desc=Texp_apply({exp_desc=Texp_ident(_, _, {val_loc; _}); _}, _); _};
                  _}],
                { exp_desc=Texp_function
                    ( _,
                      [{c_lhs={pat_desc=Tpat_var (_, _); pat_loc={loc_ghost=true; _}; _};
                        c_rhs={exp_desc=Texp_apply (_, args); exp_loc={loc_ghost=true; _}; _}; _}],
                       _);
                  exp_loc={loc_ghost=true; _}; _})
          when not val_loc.Location.loc_ghost ->
            treat_args ~anon:true val_loc args
        | Texp_function (_,
              [{c_lhs={pat_desc=Tpat_var (_, _); pat_loc={loc_ghost=true; _}; _};
                c_rhs={exp_desc=Texp_apply (_, args); exp_loc={loc_ghost=true; _}; _}; _}], _) ->
            treat_args !last_loc args
        | _ -> () end
      | _ -> ())
    args


(* Look for bad style typing *)
let rec check_type t loc = if !(!style_flag.sub1) then match t.desc with
  | Tarrow (lab, _, t, _) -> begin match lab with
    | Optional _ (*when !(!style_flag.sub1)*) ->
        style := (!current_src, loc, "val f: ... -> (... -> ?_:_ -> ...) -> ...") :: !style
    | _ -> check_type t loc end
  | Tlink t -> check_type t loc
  | _ -> ()

(* Construct the 'opt_args' list of func in node *)
let rec build_node_args node expr = match expr.exp_desc with
  | Texp_function (lab, [{c_lhs={pat_desc=Tpat_var(_, _); pat_type; _}; c_rhs=exp; _}], _) ->
      check_type pat_type expr.exp_loc;
      (function
          | Asttypes.Optional s ->
              node.func.opt_args <- s::node.func.opt_args;
            build_node_args node exp
          | _ -> node.func.need <- node.func.need + 1
      ) lab
  | Texp_apply({exp_desc=Texp_ident(_, _, {val_loc=loc2; _}); _}, args) ->
      treat_args loc2 args;
      merge_locs ~force:true ~search:next_fn_node node.loc loc2
  | Texp_ident(_, _, {val_loc=loc2; _}) ->
      merge_locs ~force:true ~search:next_fn_node node.loc loc2
  | _ -> ()


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
      if u = unit val_loc.Location.loc_start.Lexing.pos_fname then
        vds := (!current_src, id :: path, val_loc) :: !vds
  | Sig_module (id, {Types.md_type = t; _}, _) -> List.iter (collect_export (id :: path) u) (sign t)
  | _ -> ()


let is_unit t =
  match (Ctype.repr t).desc with
  | Tconstr (p, [], _) -> Path.same p Predef.path_unit
  | _ -> false

(* Get the last element of a list wrapped in a list *)
let tip =
  List.fold_left (fun _ e -> [e]) []

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
      vb_expr={exp_desc=(Texp_function _ | Texp_apply _); _} as exp;
      _
    } ->
      build_node_args (vd_node ~name:(Ident.name id) loc1) exp
  | {
      vb_pat={pat_desc=Tpat_var(id, {loc; _}); _};
      _
    } when not loc.Location.loc_ghost ->
      vd_node ~name:(Ident.name id) loc |> ignore
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
    | Tstr_value (_, l) -> begin match tip l with
      | [vb] -> value_binding vb
      | _ -> () end
    | _ -> () end;
    super.structure_item self i
  in
  let pat self p =
    let u s = style := (!current_src, p.pat_loc, Printf.sprintf "unit pattern %s" s) :: !style in
    begin if is_unit p.pat_type && !(!style_flag.sub2) then match p.pat_desc with
      | Tpat_construct _ -> ()
      | Tpat_var (_, {txt = "eta"; loc = _}) when p.pat_loc = Location.none -> ()
      | Tpat_var (_, {txt; loc = _}) -> u txt
      | Tpat_any -> u "_"
      | _ -> u ""
    end;
    super.pat self p
  in
  let expr self e =
    begin match e.exp_desc with
    | Texp_ident (_, _, {Types.val_loc; _}) when not val_loc.Location.loc_ghost ->
        Hashtbl.add references val_loc e.exp_loc;
        (vd_node val_loc).func.used <- true
    | Texp_let (_, [{vb_pat; _}], _) when is_unit vb_pat.pat_type && !(!style_flag.sub3) ->
        style := (!current_src, vb_pat.pat_loc, "let () = ... in ... (=> use sequence)") :: !style
    | Texp_let (
          Nonrecursive,
          [{vb_pat = {pat_desc = Tpat_var (id1, _); pat_loc; _}; _}],
          {exp_desc= Texp_ident (Pident id2, _, _); exp_extra = []; _}) when id1 = id2 && !(!style_flag.sub4) ->
        style := (!current_src, pat_loc, "let x = ... in x (=> useless binding)") :: !style
    | Texp_apply({exp_desc = Texp_ident (_, _, {Types.val_loc; _}); _}, args) ->
        treat_args val_loc args
    | _ ->
        ()
    end;
    super.expr self e
  in
  let expr = wrap expr (fun x -> x.exp_loc) in
  let pat = wrap pat (fun x -> x.pat_loc) in
  let structure_item = wrap structure_item (fun x -> x.str_loc) in
  {super with structure_item; expr; pat}

let kind fn =
  if Filename.check_suffix fn ".cmi" then
    let base = Filename.chop_suffix fn ".cmi" in
    if Sys.file_exists (base ^ ".mfi") then `Iface (base ^ ".mfi")
    else if Sys.file_exists (base ^ ".mli") then `Iface (base ^ ".mli")
    else `Ignore
  else if Filename.check_suffix fn ".cmt" then
    let base = Filename.chop_suffix fn ".cmt" in
    if Sys.file_exists (base ^ ".mf") then `Implem (base ^ ".mf")
    else if Sys.file_exists (base ^ ".ml") then `Implem (base ^ ".ml")
    else `Ignore
  else if (try Sys.is_directory fn with _ -> false) then
    `Dir
  else `Ignore


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
  let rec norm_path xs =
    match xs with
    | [] -> []
    | x :: ((y :: _) as yss) when x = y && x = Filename.current_dir_name -> norm_path yss
    | x :: xss ->
        let yss = List.filter (fun x -> x <> Filename.current_dir_name) xss in
        x :: yss
  in
  let rec concat_path xs =
    match xs with
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
      | Some cmt ->
          begin match cmt.cmt_annots with
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
      | None -> ()
      end

  | `Dir ->
      if not (is_excluded_dir fn) then
        Array.iter
          (fun s -> if s <> ".svn" then load_file (fn ^ "/" ^ s))
          (Sys.readdir fn)
      (* else Printf.eprintf "skipping directory %s\n" fn *)

  | `Ignore ->
      ()


let analyse_opt_args () =
  let all = ref [] in
  let tbl = Hashtbl.create 256 in
  List.iter
    (fun (loc, lab, has_val, callsite) ->
      let loc = if loc.func.opt_args = [] then next_fn_node loc else loc in
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
    )
    !opt_args;
  List.sort compare !all


(* Helpers for the following sortings *)

let base_pos_info (fn, l, c) = (unit fn, l, c)

let comp_loc loc1 loc2 =
  let start loc = Location.(get_pos_info loc.loc_start) in
  compare (base_pos_info @@ start loc1) (base_pos_info @@ start loc2)

let report_opt_args l =
  let l =
    List.filter
      (fun (_, _, slot) ->
        slot.with_val = [] && !(!opt_flag.sub2)
        || slot.without_val = [] && !(!opt_flag.sub1))
      l
    |> List.fast_sort
        (fun (loc1, lab1, slot1) (loc2, lab2, slot2) ->
            let tmp = comp_loc loc1 loc2 in
            if tmp = 0 then compare (lab1, slot1) (lab2, slot2)
            else tmp)
  in
  if l <> [] then begin
    section "OPTIONAL ARGUMENTS";
    List.iter
      (fun (loc, lab, slot) ->
         prloc loc;
         Printf.printf "%s %s\n" lab (if slot.with_val = [] then "NEVER" else "ALWAYS")
      ) l;
    separator ()
  end

let report_style () =
  if !style <> [] then begin
    section "CODING STYLE";
    List.iter (fun (fn, l, s) -> prloc ~fn l; print_endline s)
      @@ List.fast_sort (fun x y -> compare (base_pos_info x) (base_pos_info y))
      !style;
    separator ()
  end

let report_unused_exported () =
  let l =
    List.filter (fun (_, _, loc) -> not (Hashtbl.mem references loc
  || try Hashtbl.mem references @@ Hashtbl.find corres loc with Not_found -> false)) !vds
    |> List.fast_sort (fun x y -> compare (base_pos_info x) (base_pos_info y))
  in
  if l <> [] then begin
    section "UNUSED EXPORTED VALUES";
    List.iter
      (fun (fn, path, loc) ->
         prloc ~fn loc;
         print_endline (String.concat "." (List.rev_map Ident.name path));
      ) l;
    separator ()
  end

(* Assumes interfaces and implementation are properly separated and basenames are uniques
 * Only accounts one module implementation per .ml *)
let report_unused () =
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
          if not node.func.used && not (exportable node) then node::l
          else l)
      vd_nodes
      []
    |> List.fast_sort
      (fun n1 n2 ->
          let tmp = comp_loc n1.loc n2.loc in
          if tmp = 0 then compare n1 n2
          else tmp)
  in
  if l <> [] then begin
    section "UNUSED VALUES";
    List.iter
      (fun node ->
         prloc node.loc;
         print_endline node.name)
      l;
    separator ()
  end

let set_all fn =
  opt_flag := {!opt_flag with sub3 = ref false; sub4 = ref false};
  exported_flag := {!exported_flag with sub2 = ref false; sub3 = ref false; sub4 = ref false};
  unused_flag := {!unused_flag with sub2 = ref false; sub3 = ref false; sub4 = ref false};
  Printf.eprintf "Scanning files...\n%!";
  load_file fn

let parse () =
  let update_all b () =
      style_flag := make_flag b; exported_flag := make_flag b; unused_flag := make_flag b; opt_flag := make_flag b
  in

  let rec update_flag b flag s =
    let comp = if String.length s > 3 then String.sub s 0 2 else s in
    match comp with
      | "+1" -> !flag.sub1 := b;
          update_flag b flag @@ String.sub s (String.length comp) (String.length s - String.length comp)
      | "+2" -> !flag.sub2 := b;
          update_flag b flag @@ String.sub s (String.length comp) (String.length s - String.length comp)
      | "+3" -> !flag.sub3 := b;
          update_flag b flag @@ String.sub s (String.length comp) (String.length s - String.length comp)
      | "+4" -> !flag.sub4 := b;
          update_flag b flag @@ String.sub s (String.length comp) (String.length s - String.length comp)
      | "all"  -> flag := make_flag b
      | "" -> ()
      |_ -> raise @@ Arg.Bad ("unknwon option: " ^ s)
  in

  Arg.(parse
    [ "--exclude-directory", String exclude_dir, "<directory>  Exclude given directory from research.";
      "--verbose", Unit set_verbose, "Verbose mode (ie., show scanned files)";
      "-v", Unit set_verbose, "Verbose mode (ie., show scanned files)";

      "-a", Unit (update_all false), " Disable all warnings";
      "--nothing", Unit (update_all false), " Disable all warnings";
      "-A", Unit (update_all true), " Enable all warnings";
      "--all", Unit (update_all true), " Enable all warnings";

      "-e", Unit (fun () -> !exported_flag.sub1 := false), " Disable unused exported values warnings";
      "-E", Unit (fun () -> !exported_flag.sub1 := true), " Enable unused exported values warnings";

      "-o", String (update_flag false opt_flag),
        " Disable optional arguments warnings. Options:\n\
          \tSee -O";
      "-O", String (update_flag true opt_flag),
        " Enable optional arguments warnings. Options (can be used together):\n\
          \t+1: ALWAYS\n\
          \t+2: NEVER\n\
          \tall: +1+2";

      "-s", String (update_flag false style_flag),
        " Disable coding style warnings. Options:\n\
          \tSee -S";
      "-S", String (update_flag true style_flag),
        " Enable coding style warnings. Options (can be used together):\n\
          \t+1: optional arg in arg\n\
          \t+2: unit pattern\n\
          \t+3: use sequence\n\
          \t+4: useless binding\n\
          \tall: +1+2+3+4";

      "-u", Unit (fun () -> !unused_flag.sub1 := false), " Disable unused values warnings";
      "-U", Unit (fun () -> !unused_flag.sub1 := true), " Enable unused values warnings";
    ]
    set_all
    ("Usage: " ^ Sys.argv.(0) ^ " <options> <directory|file>\nOptions are:"))


let () =
  try
    parse ();
    Printf.eprintf " [DONE]\n\n%!";

    if (status_flag !unused_flag)    then  report_unused ();
    if (status_flag !exported_flag)  then  report_unused_exported ();
    if (status_flag !opt_flag)       then  report_opt_args (analyse_opt_args ());
    if (status_flag !style_flag)     then  report_style ();

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
