(***************************************************************************)
(*  Copyright (C) 2014-2015 LexiFi SAS. All rights reserved.               *)
(***************************************************************************)

open Types
open Typedtree

let abspath = Hashtbl.create 16
let bad_files = ref []

let vds = ref []  (* all exported value declarations *)
let references = Hashtbl.create 256  (* all value references *)

let unit fn = Filename.chop_extension (Filename.basename fn)


type vd_node =
  {
    loc: Location.t;
    mutable ptr: vd_node;
    implem: bool;
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
  else repr n.ptr

let vd_node loc =
  assert(not loc.Location.loc_ghost);
  try repr (Hashtbl.find vd_nodes loc)
  with Not_found ->
    let fn = loc.Location.loc_start.Lexing.pos_fname in
    let implem = Filename.check_suffix fn ".mf" || Filename.check_suffix fn ".ml"  in
    let rec r = {loc; ptr = r; implem} in
    Hashtbl.add vd_nodes loc r;
    r

let merge_nodes n1 n2 =
  let n1 = repr n1 and n2 = repr n2 in
  if n1.implem && not n2.implem then n2.ptr <- n1
  else if n2.implem && not n1.implem then n1.ptr <- n2
  else if n1.loc < n2.loc then n2.ptr <- n1 else n1.ptr <- n2

let merge_locs l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then
    merge_nodes (vd_node l1) (vd_node l2)

let rec sign = function
  | Mty_signature sg -> sg
  | Mty_functor (_, _, t) -> sign t
  | Mty_ident _ | Mty_alias _ -> []

let rec collect_export path u = function
  | Sig_value (id, {Types.val_loc; _}) when not val_loc.Location.loc_ghost ->
      (* a .cmi file can contain locations from other files.
         For instance:
             module M : Set.S with type elt = int
         will create value definitions whole location is in set.mli
      *)
      if u = unit val_loc.Location.loc_start.Lexing.pos_fname then
        vds := (id :: path, val_loc) :: !vds
  | Sig_module (id, {Types.md_type = t; _}, _) -> List.iter (collect_export (id :: path) u) (sign t)
  | _ -> ()

let collect_references =
  let super = Tast_mapper.default in
  let structure_item self i =
    begin match i.str_desc with
      | Tstr_value (_, [
          {
            vb_pat={pat_desc=Tpat_var(_, {loc = loc1; _}); _};
            vb_expr={exp_desc=Texp_ident(_, _, {val_loc=loc2; _}); _};
            _
          }
        ]) ->
        merge_locs loc1 loc2
      | _ ->
        ()
    end;
    super.structure_item self i
  in
  let expr self e =
    begin match e.exp_desc with
    | Texp_ident (_, _, {Types.val_loc; _})
      when not val_loc.Location.loc_ghost ->
        Hashtbl.add references val_loc e.exp_loc
    | Texp_apply({exp_desc = Texp_ident (_, _, {Types.val_loc; _}); _}, args)
      when not val_loc.Location.loc_ghost ->
        let loc = vd_node val_loc in

        List.iter
          (function
            | (Asttypes.Optional lab, Some e, _) ->
                let has_val =
                  match e.exp_desc with
                  | Texp_construct(_,{cstr_name="None";_},_) -> false
                  | _ -> true
                in
                opt_args := (loc, lab, has_val, e.exp_loc) :: !opt_args
            | _ -> ()
          )
          args

    | _ ->
        ()
    end;
    super.expr self e
  in
  {super with structure_item; expr}

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


let regabs fn =
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

let prloc (loc : Location.t) =
  print_string (Hashtbl.find abspath loc.loc_start.pos_fname);
  print_char ':';
  print_int loc.loc_start.pos_lnum;
  print_string ": "

let report_unused (path, loc) =
  if not (Hashtbl.mem references loc) then begin
    prloc loc;
    print_string (String.concat "." (List.rev_map Ident.name path));
    print_char ' ';
    print_endline "unused"
  end

let report_opt_arg (loc, lab, slot) =
  match slot with
  | {with_val = []; without_val = _} ->
      prloc loc;
      Printf.printf "%s NEVER used\n" lab
  | {with_val = _; without_val = []} ->
      prloc loc;
      Printf.printf "%s always used\n" lab
  | _ ->
      ()

let analyse_opt_args () =
  let all = ref [] in
  let tbl = Hashtbl.create 256 in
  List.iter
    (fun (loc, lab, has_val, callsite) ->
       let loc = repr loc in
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
  List.iter report_opt_arg (List.sort compare !all)

let () =
  try
    Printf.eprintf "Scanning files...%!";
    Arg.parse
      ["--exclude-directory", Arg.String exclude_dir, "Exclude given directory from research."]
      load_file "unused_exported_values";
    Printf.eprintf " [DONE]\n%!";
    List.iter report_unused !vds;

    Printf.eprintf "Analyze optional arguments...%!";
    analyse_opt_args ();
    Printf.eprintf " [DONE]\n%!";

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
