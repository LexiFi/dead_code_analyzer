(***************************************************************************)
(*  Copyright (C) 2014-2015 LexiFi SAS. All rights reserved.               *)
(***************************************************************************)

open Types
open Typedtree

let abspath = Hashtbl.create 16
let bad_files = ref []

let vds = ref []  (* all exported value declarations *)
let references = Hashtbl.create 256  (* all value references *)

let style = ref [] (* patterns of type unit which are not () *)
let last_loc = ref Location.none
    (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""


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



type vd_node =
  {
    name: string;
    loc: Location.t;
    mutable ptr: vd_node;
    implem: bool;
    mutable args: string list;
    mutable need: int;
    mutable used: bool;
  }

let opt_args = ref []

type opt_arg =
  {
    mutable with_val: Location.t list;
    mutable without_val: Location.t list;
  }

let vd_nodes = Hashtbl.create 256

let rec repr n =
  if n.ptr == n then n
  else (if n.need = 0 then
      n.used <- true;
    repr n.ptr)

let rec next_fn_node n =
  if n.ptr == n || n.ptr.args <> [] || n.ptr.need > 0 then n.ptr
  else (if n.need = 0 then
      n.used <- true;
    next_fn_node n.ptr)

let vd_node ?(name = "_unknown_") loc =
  assert (not loc.Location.loc_ghost);
  try (Hashtbl.find vd_nodes loc)
  with Not_found ->
    let fn = loc.Location.loc_start.Lexing.pos_fname in
    let implem = Filename.check_suffix fn ".mf" || Filename.check_suffix fn ".ml"  in
    let rec r = {name; loc; ptr = r; implem; args = []; need = 0; used = false} in
    Hashtbl.add vd_nodes loc r;
    r

let merge_nodes n1 n2 =
  let n1 = repr n1 and n2 = repr n2 in
  if n1.implem && not n2.implem then n2.ptr <- n1
  else if n2.implem && not n1.implem then n1.ptr <- n2
  else if n1.loc < n2.loc then n2.ptr <- n1 else n1.ptr <- n2;
  n1.used <- true;
  n2.used <- true

let merge_nodes_f ~search n1 n2 =
  let n1 = search n1 and n2 = search n2 in
  if n2.need = 0 then n2.used <- true;
  n1.ptr <- n2

let merge_locs l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then
    merge_nodes (vd_node l1) (vd_node l2)

let merge_locs_f ~search ?name l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then
    merge_nodes_f ~search (vd_node ?name l1) (vd_node l2)

let rec treat_args val_loc args =
  let loc = vd_node val_loc in
  let tbl = Hashtbl.create 256 in
  let use = ref 0 in
  List.iter
    (function
      | (Asttypes.Optional lab, Some e, _) ->
          let has_val =
            match e.exp_desc with
              | Texp_construct(_,{cstr_name="None";_},_) -> false
              | _ -> true
          in
          let occur = ref (
            try Hashtbl.find tbl lab + 1
            with Not_found -> Hashtbl.add tbl lab 1; 1)
          in
          let count x l = List.length @@ List.find_all (( =) x) l in
          let rec locate loc =
            let count = if loc == loc.ptr then 0 else count lab loc.args in
            if loc == loc.ptr || count >= !occur then loc
            else (occur := !occur - count; locate @@ next_fn_node loc)
          in
          opt_args := (locate loc, lab, has_val, e.exp_loc) :: !opt_args
      | (_, Some e, _)  ->
          begin match e.exp_desc with
            | Texp_apply ({exp_desc=Texp_ident(_, _, {val_loc; _}); _}, args) ->
                if not val_loc.Location.loc_ghost then
                  treat_args val_loc args; (vd_node val_loc).used <- true
            | Texp_ident(_, _, {val_loc; _}) ->
                if not val_loc.Location.loc_ghost then
                  (vd_node val_loc).used <- true
            | _ -> ()
          end;
          incr use
      | _ -> incr use
    )
    args;
    let rec locate loc =
      if loc == loc.ptr || loc.need > 0 then loc
      else locate @@ next_fn_node loc
    in
    let rec check_application loc =
      if !use >= loc.need then (
        use := !use - loc.need;
        loc.used <- true;
        let next = locate loc in
        if not (loc == next) then
          check_application next)
    in check_application loc

let rec check_type t loc = match t.desc with
  | Tarrow (lab, _, t, _) -> begin match lab with 
    | Optional _ -> style := (!current_src, loc, "val f: ... -> (... -> ?_:_ -> ...) -> ...") :: !style
    | _ -> check_type t loc end
  | Tlink t -> check_type t loc
  | _ -> ()

let rec build_node_args node expr = match expr.exp_desc with
  | Texp_function (lab, [{c_lhs={pat_desc=Tpat_var(_, _); pat_type; _}; c_rhs=exp; _}], _) ->
      check_type pat_type expr.exp_loc;
      Asttypes.(function
          | Labelled _ | Nolabel -> node.need <- node.need + 1
          | Optional s ->
              node.args <- s::node.args; build_node_args node exp
      )
      lab
  | Texp_apply({exp_desc=Texp_ident(_, _, {val_loc=loc2; _}); _}, args) ->
      if not loc2.Location.loc_ghost then treat_args loc2 args;
      merge_locs_f ~search:next_fn_node node.loc loc2
  | Texp_ident(_, _, {val_loc=loc2}) ->
      merge_locs_f ~search:next_fn_node node.loc loc2
  | _ -> ()

let rec tip = function
  | [] -> []
  | [e] -> [e]
  | _::l -> tip l

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
  let structure_item self i =
    begin match i.str_desc with
      | Tstr_value (_, [
          {
            vb_pat={pat_desc=Tpat_var(id, {loc=loc1; _}); _};
            vb_expr={exp_desc=Texp_ident(_, _, {val_loc=loc2; _}); _};
            _
          }
        ]) ->
        merge_locs_f ~search:next_fn_node ~name:(Ident.name id) loc1 loc2
      | Tstr_value (_, l) -> begin match tip l with
        | [ { vb_pat={pat_desc=Tpat_var(id, {loc=loc1; _}); _};
              vb_expr={exp_desc=(Texp_function _ | Texp_apply _); _} as exp;
              _ } ] ->
          build_node_args (vd_node ~name:(Ident.name id) loc1) exp
        |_ -> () end
      | _ ->
        ()
    end;
    super.structure_item self i
  in
  let pat self p =
    let u s = style := (!current_src, p.pat_loc, Printf.sprintf "unit pattern %s" s) :: !style in
    begin if is_unit p.pat_type then match p.pat_desc with
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
    | Texp_ident (_, _, {Types.val_loc; _})
      when not val_loc.Location.loc_ghost ->
        Hashtbl.add references val_loc e.exp_loc
    | Texp_let (_, [{vb_pat; _}], _) when is_unit vb_pat.pat_type ->
        style := (!current_src, vb_pat.pat_loc, "let () = ... in ... (=> use sequence)") :: !style
    | Texp_let (Nonrecursive, [{vb_pat = {pat_desc = Tpat_var (id1, _); pat_loc; _}; _}], {exp_desc= Texp_ident (Pident id2, _, _); exp_extra = []; _}) when id1 = id2 ->
        style := (!current_src, pat_loc, "let x = ... in x (=> useless binding)") :: !style
    | Texp_apply({exp_desc = Texp_ident (_, _, {Types.val_loc; _}); _}, args)
    when not val_loc.Location.loc_ghost -> treat_args val_loc args
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
   two files with the same basename. *)
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
  (* Printf.eprintf "Scanning %s\n%!" fn; *)
  match kind fn with
  | `Iface src ->
      (* only consider module with an explicit interface *)
      let open Cmi_format in
      last_loc := Location.none;
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
                   merge_locs vd1.Types.val_loc vd2.Types.val_loc
                )
                cmt.cmt_value_dependencies
          | _ -> ()  (* todo: support partial_implementation? *)
          end
      | None -> ()
      end

  | `Dir ->
      if not (is_excluded_dir fn) then Array.iter (fun s -> if s <> ".svn" then load_file (fn ^ "/" ^ s)) (Sys.readdir fn)
(* else Printf.eprintf "skipping directory %s\n" fn *)

  | `Ignore ->
      ()

let analyse_opt_args () =
  let all = ref [] in
  let tbl = Hashtbl.create 256 in
  List.iter
    (fun (loc, lab, has_val, callsite) ->
      let loc = if loc.args = [] then next_fn_node loc else loc in
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

let report_opt_args l =
  let l = List.filter (fun (_, _, slot) -> slot.with_val = [] || slot.without_val = []) l in
  if l <> [] then begin
    section "OPTIONAL ARGUMENTS";
    List.iter
      (fun (loc, lab, slot) ->
         prloc loc;
         Printf.printf "%s %s\n" lab (if slot.with_val = [] then "NEVER" else "ALWAYS")
      )
      @@ List.fast_sort compare l;
    separator ();
  end

let report_style () =
  if !style <> [] then begin
    section "CODING STYLE";
    List.iter (fun (fn, l, s) -> prloc ~fn l; print_endline s)
      @@ List.fast_sort (fun (fn, path, loc) (fn2, path2, loc2) -> compare (unit fn, path, loc) (unit fn2, path2, loc2))
      !style;
    separator ();
  end

let report_unused_exported () =
  let l = List.filter (fun (_, _, loc) -> not (Hashtbl.mem references loc)) !vds in
  if l <> [] then begin
    section "UNUSED EXPORTED VALUES";
    List.iter
      (fun (fn, path, loc) ->
         prloc ~fn loc;
         print_endline (String.concat "." (List.rev_map Ident.name path));
      ) @@ List.fast_sort (fun (fn, path, loc) (fn2, path2, loc2) -> compare (unit fn, path, loc) (unit fn2, path2, loc2)) l;
    separator ();
  end

(* Assumes interfaces and implementation are properly separated and basenames are uniques
 * Only accounts one module implementation per .ml *)
let report_unused () =
  let exportable node = List.exists
    (fun (fn, path, _) -> unit fn = unit node.loc.loc_start.pos_fname && Ident.name @@ List.hd path = node.name)
    !vds in
  let l = Hashtbl.fold
    (fun _ node l -> if not node.used && not (exportable node) then node::l else l)
    vd_nodes [] in
  if l <> [] then begin
    section "UNUSED VALUES";
    List.iter
      (fun node ->
         prloc node.loc;
         print_endline node.name;
      ) @@ List.fast_sort (fun n1 n2 ->
            let make_b (fn, l, c) = (unit fn, l, c) in
            let make_bb loc = make_b @@ Location.get_pos_info loc in
            compare (make_bb n1.loc.loc_start) (make_bb n2.loc.loc_start)) l;
    separator ();
  end


let () =
  try
    Printf.eprintf "Scanning files...%!";
    Arg.parse
      ["--exclude-directory", Arg.String exclude_dir, "Exclude given directory from research."]
      load_file "unused_exported_values";
    Printf.eprintf " [DONE]\n%!";

    report_unused ();
    report_unused_exported ();
    report_opt_args (analyse_opt_args ());
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
