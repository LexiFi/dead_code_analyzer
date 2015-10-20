(***************************************************************************)
(*  Copyright (C) 2014-2015 LexiFi SAS. All rights reserved.               *)
(***************************************************************************)

(** Dead code anlyzing tool. It only reports unused exported values by default.
 * Options can enable reporting of optional arguments always/never used as bad style of code.
 * In addition to selecting which reports are to be displayed, the limit of authorized
 * occurences needed to be reported can be selected (default is 0).
 * It assumes .mli are compiled with -keep-locs and .ml are compiled with -bin-annot.
 *)

open Types
open Typedtree


                (********   FLAGS   ********)

let list_of_opt str =
  let rec split acc pos len =
    if pos = 0 then (str.[pos] = '+', String.sub str (pos + 1) len) :: acc
    else if str.[pos] <> '+' && str.[pos] <> '-' then split acc (pos - 1) (len + 1)
    else split ((str.[pos] = '+', String.sub str (pos + 1) len) :: acc) (pos - 1) 0
  in split [] (String.length str - 1) 0


type flexibility = {exceptions: int; percentage: float; optional: [`Percent | `Both]}
let flexibility = ref
  {
    exceptions = 0;
    percentage = 1.;
    optional = `Percent
  }

let update_flexibility s =
  let rec aux l = match l with
    | (_, "both")::l ->
        flexibility := {!flexibility with optional = `Both};
        aux l
    | (_, "percent")::l ->
        flexibility := {!flexibility with optional = `Percent};
        aux l
    | (_, s)::l -> begin try
          begin try flexibility := {!flexibility with exceptions = int_of_string s}
          with Failure _ -> flexibility := {!flexibility with percentage = float_of_string s} end;
          if !flexibility.exceptions < 0 then
            raise (Arg.Bad ("--flexibility: number of exceptions must be >= 0"))
          else if !flexibility.percentage > 1. || !flexibility.percentage < 0. then
            raise (Arg.Bad ("--flexibility: percentage must be >= 0.0 and <= 1.0"))
          else aux l
        with Failure _ -> raise (Arg.Bad ("--flexibility: unknown option: " ^ s)) end;
    | [] -> ()
  in aux (list_of_opt s)


type opt_flag = {always: bool; never: bool; call_sites: bool}
let opt_flag = ref
  {
    always = false;
    never = false;
    call_sites = false
  }

let update_opt_flag s =
  let rec aux = function
    | (b, "always")::l -> opt_flag := {!opt_flag with always = b};
        aux l
    | (b, "never")::l -> opt_flag := {!opt_flag with never = b};
        aux l
    | (b, "calls")::l -> opt_flag := {!opt_flag with call_sites = b};
        aux l
    | (b, "all")::l -> opt_flag := {!opt_flag with never = b; always = b};
        aux l
    | (_, "")::l -> aux l
    | (_, s)::_ -> raise (Arg.Bad ("-O: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


type style_flag = {opt_arg: bool; unit_pat: bool; seq: bool; binding: bool}
let style_flag = ref
  {
    opt_arg = false;
    unit_pat = false;
    seq = false;
    binding = false;
  }

let update_style_flag s =
  let rec aux = function
    | (b, "opt")::l -> style_flag := {!style_flag with opt_arg = b};
        aux l
    | (b, "unit")::l -> style_flag := {!style_flag with unit_pat = b};
        aux l
    | (b, "seq")::l -> style_flag := {!style_flag with seq = b};
        aux l
    | (b, "bind")::l -> style_flag := {!style_flag with binding = b};
        aux l
    | (b, "all")::l -> style_flag := {unit_pat = b; opt_arg = b; seq = b; binding = b};
        aux l
    | (_, "")::l -> aux l
    | (_, s)::_ -> raise (Arg.Bad ("-S: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


type exported_flag = {print: bool; call_sites: bool}
let exported_flag = ref
  {
    print = true;
    call_sites = false;
  }

let update_exported_flag s =
  let rec aux = function
    | (b, "all")::l -> exported_flag := {!exported_flag with print = b};
        aux l
    | (b, "calls")::l -> opt_flag := {!opt_flag with call_sites = b};
        aux l
    | (_, "")::l -> aux l
    | (_, s)::_ -> raise (Arg.Bad ("-S: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


let update_call_sites s =
  let rec aux l = match l with
    | (b, "E")::l -> exported_flag := {!exported_flag with call_sites = b}; aux l
    | (b, "O")::l -> opt_flag := {!opt_flag with call_sites = b}; aux l
    | (b, "all")::l ->
        exported_flag := {!exported_flag with call_sites = b};
        opt_flag := {!opt_flag with call_sites = b};
        aux l
    | (_, s)::_ -> raise (Arg.Bad ("--call-sites: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


let verbose = ref false
let set_verbose () = verbose := true

(* Print name starting with '_' *)
let underscore = ref false
let set_underscore () = underscore := true


                (********   ATTRIBUTES   ********)

let abspath = Hashtbl.create 16         (* abspath: filename's basename -> absolute path *)
let bad_files = ref []

let vds = ref []                        (* all exported value declarations *)
let type_dependencies = ref []           (* like the cmt value_dependencies but for types *)
let references = Hashtbl.create 256     (* all value references *)
let fields = Hashtbl.create 256         (* link from field paths and nodes *)
let corres = Hashtbl.create 256         (* link from dec to def *)

let style = ref []                      (* patterns of type unit which are not () *)
let last_loc = ref Location.none        (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""


                (********   HELPERS   ********)

let unit fn = Filename.chop_extension (Filename.basename fn)

let check_underscore name = not !underscore || name.[0] <> '_'

(* Section printer:
 * section:     `.> SECTION: '
 *              `==========='
 * subsection:  `.>->  SUBSECTION: '
 *              `~~~~~~~~~~~~~~~~~' *)
let section ?(sub = false) s =
  Printf.printf "%s %s:\n%s\n"
    (if sub then ".>-> " else ".>")
    s
    (String.make ((if sub then 5 else 2) + String.length s + 1) (if sub then '~' else '='))

(* End of section *)
let separator () =
  Printf.printf "%s\n\n\n" (String.make 80 '-')

(* Location printer: `filename:line: ' *)
let prloc ?fn (loc : Location.t) = begin match fn with
  | Some s when Filename.chop_extension (loc.loc_start.pos_fname) = Filename.chop_extension (Filename.basename s) ->
      print_string (Filename.dirname s ^ "/" ^ loc.loc_start.pos_fname)
  | _ -> begin match Hashtbl.find abspath loc.loc_start.pos_fname with
    | s -> print_string s
    | exception Not_found -> match fn with
      | None -> Printf.printf "!!UNKNOWN<%s>!!%!" loc.loc_start.pos_fname
      | Some s -> print_string s
    end
  end;
  print_char ':';
  print_int loc.loc_start.pos_lnum;
  print_string ": "


                (********   TYPES   *********)

type vd_node =
  {
    loc: Location.t;
    name: string;
    mutable opt_args: string list;
    mutable ptr: vd_node;               (* points to itself if not a binding to another known value *)
    implem: bool;
  }

let opt_args = ref []

type opt_arg =
  {
    mutable with_val: Location.t list;
    mutable without_val: Location.t list;
  }

let vd_nodes = Hashtbl.create 256


                (********   NODE MANIPULATION   ********)

(* Go deeper in the vd_node until cond is respected (or cannot go deeper) *)
let rec repr ?(cond = (fun _ -> false)) n =
  if n.ptr == n || cond n then n
  else repr n.ptr

(* repr specialization to get the next node corresponding to a function *)
let next_fn_node n =
  repr ~cond:(fun n -> n.opt_args <> []) n.ptr

(* Get or create a vd_node corresponding to the location *)
let vd_node ?(name = "_unknown_") loc =
  assert (not loc.Location.loc_ghost);
  try (Hashtbl.find vd_nodes loc)
  with Not_found ->
    let fn = loc.Location.loc_start.Lexing.pos_fname in
    let implem = Filename.check_suffix fn ".ml" in
    let rec r = {name; loc; ptr = r; implem; opt_args = []} in
    if name <> "_unknown_" then
      Hashtbl.add vd_nodes loc r;
    r

(* Merge nodes in order *)
let merge_nodes ~search n1 n2 =
  let n1 = search n1 and n2 = search n2 in
  n1.ptr <- n2

(* Locations l1 and l2 are part of a binding from one to another *)
let merge_locs ~search ?name l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then
    merge_nodes ~search (vd_node ?name l1) (vd_node l2)


                (********   PROCESSING   ********)

(* Verify the optional args calls. Treat args *)
let rec treat_args ?(anon = false) val_loc args =
  List.iter                               (* treat each arg's expression before all (even if ghost) *)
    (function
      | (_, None, _) -> ()
      | (_, Some e, _) -> check_args val_loc e)
    args;
  if val_loc.Location.loc_ghost then ()   (* Ghostbuster *)
  else begin                              (* else: `begin ... end' for aesthetics *)
    let loc = vd_node val_loc in
    let tbl = Hashtbl.create 256 in       (* tbl: label -> nb of occurences *)

    let treat lab expr =
      let has_val = match expr with
        | None -> anon
        | Some {exp_desc=Texp_construct (_, {cstr_name="None"; _}, _); _} -> false
        | _ -> true
      in
      let occur = ref @@
        try Hashtbl.find tbl lab + 1
        with Not_found -> Hashtbl.add tbl lab 1; 1
      in
      let count x l = List.length @@ List.find_all (( = ) x) l in
      let rec locate loc =
        let count = if loc == loc.ptr then 0 else count lab loc.opt_args in
        if loc == loc.ptr || count >= !occur then loc
        else (occur := !occur - count; locate @@ next_fn_node loc)
      in
      if check_underscore lab then
        opt_args :=
          (locate loc, lab, has_val, (match expr with
            | Some e when not e.exp_loc.Location.loc_ghost -> e.exp_loc
            | _ -> !last_loc))
          :: !opt_args
    in

    List.iter
      (function
        | (Asttypes.Optional lab, expr, _) when (expr <> None || not anon) ->
          treat lab expr
        | _ -> ())
      args
  end

(* Verify the nature of the argument to detect and treat function applications and uses *)
and check_args call_site e =
  let call_site =
    if call_site.Location.loc_ghost then  e.exp_loc
    else            (* default *)         call_site
  in
  (* Optional arguments used top match a signature are considered used *)
  let rec get_sig_args args typ = match typ.desc with
    | Tarrow (Asttypes.Optional _ as arg, _, t, _) ->
        get_sig_args ((arg, Some {e with exp_desc=Texp_constant (Asttypes.Const_int 0)}, Optional)::args) t
    | Tlink t -> get_sig_args args t
    | _ -> args

  in match e.exp_desc with

    | Texp_function (_,
          [{c_lhs={pat_desc=Tpat_var (_, _); pat_loc={loc_ghost=true; _}; _};
            c_rhs={exp_desc=Texp_apply (_, args); exp_loc={loc_ghost=true; _}; _}; _}], _) ->
        treat_args call_site args

    | Texp_apply ({exp_desc=Texp_ident (_, _, {val_loc; _}); _}, args)
    | Texp_apply ({exp_desc=Texp_field (_, _, {lbl_loc=val_loc; _}); _}, args) ->
        treat_args val_loc (get_sig_args args e.exp_type);
        if not val_loc.Location.loc_ghost then
          last_loc := val_loc

    | Texp_ident (_, _, {val_loc; _}) ->
        treat_args val_loc (get_sig_args [] e.exp_type)

    | Texp_let (* Partial application as argument may cut in two parts:
                * let _ = partial in implicit opt_args elimination *)
        ( _,
          [{vb_expr={exp_desc=Texp_apply ({exp_desc=Texp_ident (_, _, {val_loc; _}); _}, _); _}; _}],
          { exp_desc=Texp_function (_,
              [{c_lhs={pat_desc=Tpat_var (_, _); pat_loc={loc_ghost=true; _}; _};
                c_rhs={exp_desc=Texp_apply (_, args); exp_loc={loc_ghost=true; _}; _}; _}],_);
            exp_loc={loc_ghost=true; _};_}) ->
        treat_args ~anon:true val_loc args

    | _ -> ()


(* Go down the exp to apply args on every "child". Used for conditional branching *)
let rec treat_exp exp args = match exp.exp_desc with
  | Texp_ident (_, _, {Types.val_loc; _})
  | Texp_field (_, _, {lbl_loc=val_loc; _}) ->
      treat_args ~anon:true val_loc args
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


(* Look for bad style typing *)
let rec check_type t loc = if !style_flag.opt_arg then match t.desc with
  | Tlink t -> check_type t loc
  | Tarrow (lab, _, t, _) -> begin match lab with
    | Optional lab when check_underscore lab ->
        style := (!current_src, loc, "val f: ... -> (... -> ?_:_ -> ...) -> ...") :: !style
    | _ -> check_type t loc end
  | _ -> ()


(* Construct the 'opt_args' list of func in node *)
let rec build_node_args node expr = match expr.exp_desc with
  | Texp_function (lab, [{c_lhs={pat_type; _}; c_rhs=exp; _}], _) ->
      check_type pat_type expr.exp_loc;
      begin match lab with
        | Asttypes.Optional s ->
            node.opt_args <- s::node.opt_args;
            build_node_args node exp
        | _ -> () end
  | Texp_apply ({exp_desc=Texp_ident (_, _, {val_loc=loc2; _}); _}, args)
  | Texp_apply ({exp_desc=Texp_field (_, _, {lbl_loc=loc2; _}); _}, args) ->
      treat_args loc2 args;
      merge_locs ~search:next_fn_node node.loc loc2
  | Texp_ident (_, _, {val_loc=loc2; _}) ->
      merge_locs ~search:next_fn_node node.loc loc2
  | _ -> ()


let is_unit t = match (Ctype.repr t).desc with
  | Tconstr (p, [], _) -> Path.same p Predef.path_unit
  | _ -> false


(* Binding in Tstr_value *)
let value_binding = function
  | {
      vb_pat={pat_desc=Tpat_var (id, {loc=loc1; _}); _};
      vb_expr={exp_desc=Texp_ident (_, _, {val_loc=loc2; _}); _};
      _
    } ->
      merge_locs ~search:next_fn_node ~name:(Ident.name id) loc1 loc2
  | {
      vb_pat={pat_desc=Tpat_var (id, {loc=loc1; _}); _};
      vb_expr=exp;
      _
    } when not loc1.loc_ghost ->
      build_node_args (vd_node ~name:(Ident.name id) loc1) exp
  | _ -> ()

let mods = ref [] (* module path *)

(* declarations Tstr_type *)
let ttype typ = match typ.typ_kind with
  | Ttype_record l ->
      List.iter
        (fun lab ->
          let path = String.concat "." @@ List.rev @@
            lab.ld_name.Asttypes.txt
            :: typ.typ_name.Asttypes.txt :: !mods
            @ (String.capitalize_ascii (unit !current_src):: [])
          in
          begin try match typ.typ_manifest with
            | Some {ctyp_desc=Ttyp_constr (_, {txt;  _}, _); _} ->
                let loc = Hashtbl.find fields
                  (String.concat "." @@
                    String.capitalize_ascii (unit !current_src)
                    :: Longident.flatten txt
                    @ (lab.ld_name.Asttypes.txt :: []))
                in
                let loc2 = Hashtbl.find fields path in
                let _, mloc =
                  try List.find (fun (_, l) -> l = loc) (List.rev !type_dependencies)
                  with Not_found -> loc, loc
                in
                type_dependencies :=
                  (loc2, mloc) :: (loc, lab.Typedtree.ld_loc) :: !type_dependencies;
            | _ -> ()
          with _ -> () end;
          try
            let loc = Hashtbl.find fields path in
            type_dependencies := (loc, lab.Typedtree.ld_loc) :: !type_dependencies;
          with Not_found -> Hashtbl.add fields path lab.Typedtree.ld_loc)
        l
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
    | Tstr_type  (_, l) -> List.iter ttype l
    | Tstr_module  {mb_name={txt; _};_} -> mods := txt :: !mods
    | _ -> () end;
    let res = super.structure_item self i in
    begin match i.str_desc with
    | Tstr_module _ -> mods := List.tl !mods
    | _ -> () end;
    res
  in

  let pat self p =
    let u s = style := (!current_src, p.pat_loc, Printf.sprintf "unit pattern %s" s) :: !style in
    begin if is_unit p.pat_type && !style_flag.unit_pat then match p.pat_desc with (* look for unit pattern *)
      | Tpat_construct _ -> ()
      | Tpat_var (_, {txt = "eta"; loc = _}) when p.pat_loc = Location.none -> ()
      | Tpat_var (_, {txt; _})-> if check_underscore txt then u txt
      | Tpat_any -> if not !underscore then u "_"
      | _ -> u "" end;
    begin match p.pat_desc with
      | Tpat_record (l, _) ->
          List.iter
            (fun (_, lab, _) -> Hashtbl.add references lab.lbl_loc
              (p.pat_loc :: try Hashtbl.find references lab.lbl_loc with Not_found -> []))
            l
      | _ -> () end;
    super.pat self p
  in

  let expr self e = begin match e.exp_desc with   (* most of the processing starts here *)
    | Texp_apply (exp, args) -> treat_exp exp args
    | Texp_ident (_, _, {Types.val_loc; _})
    | Texp_field (_, _, {lbl_loc=val_loc; _})
      when not val_loc.Location.loc_ghost ->
        Hashtbl.add references val_loc (e.exp_loc :: try Hashtbl.find references val_loc with Not_found -> [])
    | Texp_let (_, [{vb_pat; _}], _) when is_unit vb_pat.pat_type && !style_flag.seq -> begin match vb_pat.pat_desc with
        | Tpat_var (id, _) when !underscore && (Ident.name id).[0] = '_' -> ()
        | _ -> style := (!current_src, vb_pat.pat_loc, "let () = ... in ... (=> use sequence)") :: !style end
    | Texp_let (
          Nonrecursive,
          [{vb_pat = {pat_desc = Tpat_var (id1, _); pat_loc; _}; _}],
          {exp_desc= Texp_ident (Pident id2, _, _); exp_extra = []; _})
      when id1 = id2 && !style_flag.binding && check_underscore (Ident.name id1) ->
        style := (!current_src, pat_loc, "let x = ... in x (=> useless binding)") :: !style
    | _ -> () end;
    super.expr self e
  in

  let expr = wrap expr (fun x -> x.exp_loc) in
  let pat = wrap pat (fun x -> x.pat_loc) in
  let structure_item = wrap structure_item (fun x -> x.str_loc) in
  {super with structure_item; expr; pat}



let rec collect_export path u signature =

  let export path id loc =
    if not loc.Location.loc_ghost && u = unit loc.Location.loc_start.Lexing.pos_fname
    && check_underscore (Ident.name id) then
      vds := (!current_src, id :: path, loc) :: !vds
  in
  let collect_export_type path t = match t.type_kind with
    | Type_record (l, _) ->
        List.iter
          (fun {Types.ld_id; ld_loc; _} ->
            if t.type_manifest = None then
              export path ld_id ld_loc;
            let path = String.concat "." @@ List.rev_map (fun id -> id.Ident.name) (ld_id::path) in
            if Hashtbl.mem fields path then
              Hashtbl.add corres ld_loc
                (let loc = Hashtbl.find fields path in
                loc :: try Hashtbl.find corres loc with Not_found -> []);
            Hashtbl.replace fields path ld_loc)
          l
    | _ -> ()
  in
  let rec sign = function
    | Mty_signature sg -> sg
    | Mty_functor (_, _, t) -> sign t
    | Mty_ident _ | Mty_alias _ -> []
  in

  match signature with
    | Sig_value (id, {Types.val_loc; _}) when not val_loc.Location.loc_ghost ->
        (* a .cmi file can contain locations from other files.
          For instance:
              module M : Set.S with type elt = int
          will create value definitions whose location is in set.mli
        *)
        export path id val_loc
    | Sig_type (id, t, _) ->
          collect_export_type (id::path) t
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
  Hashtbl.add abspath (Filename.basename fn) fn

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
    if !exported_flag.print then
      List.iter (collect_export [Ident.create (String.capitalize_ascii u)] u) (read_cmi fn).cmi_sign
  with Cmi_format.Error (Wrong_version_interface _) ->
    (*Printf.eprintf "cannot read cmi file: %s\n%!" fn;*)
    bad_files := fn :: !bad_files

(* Starting point *)
let rec load_file fn = match kind fn with
  | `Iface src ->
      (* only consider module with an explicit interface *)
      last_loc := Location.none;
      if !verbose then Printf.eprintf "Scanning %s\n%!" fn;
      read_interface fn src

  | `Implem src ->
      let open Cmt_format in
      last_loc := Location.none;
      if !verbose then Printf.eprintf "Scanning %s\n%!" fn;
      regabs src;
      let cmt =
        try Some (read_cmt fn)
        with _ -> bad_files := fn :: !bad_files; None
      in

      (* Used if the cmt is valid. Associates the two value dependencies *)
      let assoc (vd1, vd2) =
        let fn1 = vd1.Location.loc_start.pos_fname and fn2 = vd2.Location.loc_start.pos_fname in
        let is_implem fn = Filename.check_suffix fn ".ml" in
        let has_iface fn =
          Filename.check_suffix fn ".mli"
          || try Sys.file_exists (Filename.chop_extension (Hashtbl.find abspath fn) ^ ".mli")
          with Not_found -> false
        in
        if is_implem fn1 && is_implem fn2 then
          Hashtbl.add references vd1 (vd2 :: try Hashtbl.find references vd1 with Not_found -> [])
        else if not (is_implem fn1 && has_iface fn1) then begin
          Hashtbl.add corres vd1 (vd2 :: try Hashtbl.find corres vd1 with Not_found -> []);
          Hashtbl.add references vd1 @@ List.sort_uniq compare
            ((try Hashtbl.find references vd1 with Not_found -> [])
            @ try Hashtbl.find references vd2 with Not_found -> [])
        end
        else
          Hashtbl.add corres vd2 (vd1 :: try Hashtbl.find corres vd2 with Not_found -> [])
      in

      begin match cmt with
        | Some {cmt_annots=Implementation x; cmt_value_dependencies; _} ->
            ignore (collect_references.structure collect_references x);
            List.iter assoc (List.rev_map (fun (vd1, vd2) -> (vd1.Types.val_loc, vd2.Types.val_loc)) cmt_value_dependencies);
            List.iter assoc !type_dependencies
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
let analyse_opt_args () =
  let all = ref [] in
  let tbl = Hashtbl.create 256 in

  let analyse = fun (loc, lab, has_val, callsite) ->
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
  List.iter                   (* Remove call sites accounted more than once for the same element *)
    (fun (_, _, slot) ->
      slot.with_val     <- List.sort_uniq compare slot.with_val;
      slot.without_val  <- List.sort_uniq compare slot.without_val)
    !all;
  !all


        (**** Helpers for the following reports ****)

(* Absolute path *)
let abs loc = match Hashtbl.find abspath loc.Location.loc_start.pos_fname with
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

let percent base = 1. -. (float_of_int base) *. (1. -. !flexibility.percentage) /. 10.

(* Base pattern for reports *)
let report s ?(extra = "Called") l continue nb_call pretty_print reporter =
  if nb_call = 0 || l <> [] then begin
    section ~sub:(nb_call <> 0)
    @@ (if nb_call = 0 then s
        else if !flexibility.optional = `Both || extra = "Called" then
          Printf.sprintf "%s: %s %d time(s)" s extra nb_call
        else Printf.sprintf "%s: at least %3.2f%% of the time" s (100. *. percent nb_call));
    List.iter pretty_print l;
    if continue nb_call then
      (if l <> [] then print_endline "--------" else ()) |> print_newline |> print_newline
  end;
  if continue nb_call then reporter (nb_call + 1)
  else (print_newline (); print_endline "Nothing else to report in this section" |> separator)


let report_opt_args s l =
  let rec report_opt_args nb_call =
    let l = List.filter
        (fun (_, _, slot, ratio, _) -> let ratio = 1. -. ratio in
          if !flexibility.optional = `Both then
            ratio >= !flexibility.percentage && check_length nb_call slot
          else ratio >= percent nb_call
            && (!flexibility.percentage >= 1. || ratio < (percent (nb_call - 1))))
      @@ List.map
        (fun (loc, lab, slot) ->
          let l = if s = "NEVER" then slot.with_val else slot.without_val in
          let total = List.length slot.with_val + List.length slot.without_val in
          let ratio = float_of_int (List.length l) /. float_of_int total
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
      prloc loc; print_string ("?" ^ lab);
      if ratio <> 0. then begin
        Printf.printf "   (%d/%d calls)" (total - List.length slot) total;
        if !opt_flag.call_sites then print_string "  Exceptions:"
      end;
      print_newline ();
      if !opt_flag.call_sites && ratio <> 1. then begin
        List.iter (pretty_print_call ()) slot;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call =
      !flexibility.optional = `Both && nb_call < !flexibility.exceptions
      || !flexibility.optional = `Percent && percent nb_call > !flexibility.percentage
    in
    let s =
      (if nb_call > 0 then "OPTIONAL ARGUMENTS: ALMOST "
      else "OPTIONAL ARGUMENTS: ") ^ s
    in
    report s ~extra:"Except" l continue nb_call pretty_print report_opt_args;

  in report_opt_args 0


let report_unused_exported () =
  let rec report_unused_exported nb_call =
    let l =
      let folder = fun acc (fn, path, loc) ->
        let l = ref [] in
        let test loc =
          Hashtbl.mem references loc
          && not @@ check_length nb_call @@ (l := Hashtbl.find references loc; !l)
        in
        match not (test loc || (let loc = Hashtbl.find corres loc in
              List.fold_left (fun res node -> res || test node) false loc))
              && check_length nb_call !l with
          | exception Not_found when nb_call = 0 -> (fn, path, loc, !l)::acc
          | exception Not_found -> acc
          | true -> (fn, path, loc, !l)::acc
          | false -> acc
      in
      List.fold_left folder [] !vds
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
      print_string (String.concat "." @@ List.tl @@ (List.rev_map Ident.name path));
      if call_sites <> [] && !exported_flag.call_sites then print_string "    Call sites:";
      print_newline ();
      if !exported_flag.call_sites then begin
        List.iter (pretty_print_call ()) call_sites;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call = nb_call < !flexibility.exceptions in
    let s = if nb_call = 0 then "UNUSED EXPORTED VALUES" else "ALMOST UNUSED EXPORTED VALUES" in
    report s l continue nb_call pretty_print report_unused_exported

  in report_unused_exported 0


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



(* Option parsing and processing *)
let parse () =
  let update_all b () =
    update_style_flag (b ^ "all");
    update_exported_flag (b ^ "all");
    update_opt_flag (b ^ "all")
  in

  (* any extra argument can be accepted by any option using some
   * although it doesn't necessary affects the results (e.g. -O 3+4) *)
  Arg.(parse
    [ "--exclude-directory", String exclude_dir, "<directory>  Exclude given directory from research.";

      "--no-underscore", Unit set_underscore, " Hide names starting with an underscore";

      "--verbose", Unit set_verbose, " Verbose mode (ie., show scanned files)";
      "-v", Unit set_verbose, " Verbose mode (ie., show scanned files)";

      "--flexibility", String update_flexibility,
        " Reports values that are almost in a category.\n    \
          Delimiters '+' and '-' can both be used.\n    \
          Options (can be used together):\n\
          \t<integer>: Maximum number of exceptions. Default is 0.\n\
          \t<float>: Minimum percentage (between 0.0 and 1.0) of valid cases (for optional arguments). Default is 1.0.\n\
          \tpercent: Optional arguments have to respect the percentage only. Default behaviour\n\
          \tboth: Optional arguments have to respect both constraints";

      "--call-sites", String update_call_sites,
        " Reports call sites for exceptions in the given category (only useful when used with the flexibility option).\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
          Options (can be used together):\n\
          \tE: Equivalent to -E +calls.\n\
          \tO: Equivalent to -O +calls.\n\
          \tall: O & E";

      "-a", Unit (update_all "-"), " Disable all warnings";
      "--nothing", Unit (update_all "-"), " Disable all warnings";
      "-A", Unit (update_all "+"), " Enable all warnings";
      "--all", Unit (update_all "+"), " Enable all warnings";

      "-E", String (update_exported_flag),
        " Enable/Disable unused exported values warnings.\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
        Options (can be used together):\n\
          \tall\n\
          \tcalls: show call sites";

      "-O", String (update_opt_flag),
        " Enable/Disable optional arguments warnings.\n    \
        Delimiters '+' and '-' determine if the following option is to enable or disable.\n    \
        Options (can be used together):\n\
          \talways\n\
          \tnever\n\
          \tall: always & never\n\
          \tcalls: show call sites";

      "-S", String (update_style_flag),
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

    if !exported_flag.print                 then  report_unused_exported ();
    if !opt_flag.always || !opt_flag.never  then  begin let tmp = analyse_opt_args () in
                if !opt_flag.always         then  report_opt_args "ALWAYS" tmp;
                if !opt_flag.never          then  report_opt_args "NEVER" tmp end;
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
