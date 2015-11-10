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


                (********   FLAGS   ********)
module DeadFlag = struct

  let list_of_opt ?(ignore = []) str =
    try
      let rec split acc pos len =
        let jump =
          try
            List.find
            (fun s -> pos >= String.length s && String.sub str (pos - String.length s) (String.length s) = s)
            ignore
          with Not_found -> ""
        in
        if jump <> "" then
          split acc (pos - String.length jump - 1) (len + String.length jump + 1)
        else if str.[pos] <> '+' && str.[pos] <> '-' then
          split acc (pos - 1) (len + 1)
        else let acc = (str.[pos] = '+', String.trim (String.sub str (pos + 1) len)) :: acc in
          if pos > 0 then split acc (pos - 1) 0
          else acc
      in split [] (String.length str - 1) 0
    with _ -> raise (Arg.Bad ("options' arguments must start with a delimiter (`+' or `-')"))


  type threshold = {exceptions: int; percentage: float; optional: [`Percent | `Both]}
  let threshold = ref
    {
      exceptions = 0;
      percentage = 1.;
      optional = `Percent
    }

  let update_threshold s =
    let rec aux l = match l with
      | (_, "both")::l ->
          threshold := {!threshold with optional = `Both};
          aux l
      | (_, "percent")::l ->
          threshold := {!threshold with optional = `Percent};
          aux l
      | (_, s)::l -> begin try
            begin try threshold := {!threshold with exceptions = int_of_string s}
            with Failure _ -> threshold := {!threshold with percentage = float_of_string s} end;
            if !threshold.exceptions < 0 then
              raise (Arg.Bad ("--threshold: number of exceptions must be >= 0"))
            else if !threshold.percentage > 1. || !threshold.percentage < 0. then
              raise (Arg.Bad ("--threshold: percentage must be >= 0.0 and <= 1.0"))
            else aux l
          with Failure _ -> raise (Arg.Bad ("--threshold: unknown option: " ^ s)) end;
      | [] -> ()
    in aux (list_of_opt s)


  type opt = {always: bool; never: bool; call_sites: bool}
  let opt = ref
    {
      always = false;
      never = false;
      call_sites = false
    }

  let update_opt s =
    let rec aux = function
      | (b, "always")::l -> opt := {!opt with always = b};
          aux l
      | (b, "never")::l -> opt := {!opt with never = b};
          aux l
      | (b, "calls")::l -> opt := {!opt with call_sites = b};
          aux l
      | (b, "all")::l -> opt := {!opt with never = b; always = b};
          aux l
      | (_, "")::l -> aux l
      | (_, s)::_ -> raise (Arg.Bad ("-O: unknown option: " ^ s))
      | [] -> ()
    in aux (list_of_opt s)


  type style = {opt_arg: bool; unit_pat: bool; seq: bool; binding: bool}
  let style = ref
    {
      opt_arg = false;
      unit_pat = false;
      seq = false;
      binding = false;
    }

  let update_style s =
    let rec aux = function
      | (b, "opt")::l -> style := {!style with opt_arg = b};
          aux l
      | (b, "unit")::l -> style := {!style with unit_pat = b};
          aux l
      | (b, "seq")::l -> style := {!style with seq = b};
          aux l
      | (b, "bind")::l -> style := {!style with binding = b};
          aux l
      | (b, "all")::l -> style := {unit_pat = b; opt_arg = b; seq = b; binding = b};
          aux l
      | (_, "")::l -> aux l
      | (_, s)::_ -> raise (Arg.Bad ("-S: unknown option: " ^ s))
      | [] -> ()
    in aux (list_of_opt s)


  type exported = {print: bool; call_sites: bool}
  let exported = ref
    {
      print = true;
      call_sites = false;
    }

  let update_exported s =
    let rec aux = function
      | (b, "all")::l -> exported := {!exported with print = b};
          aux l
      | (b, "calls")::l -> opt := {!opt with call_sites = b};
          aux l
      | (_, "")::l -> aux l
      | (_, s)::_ -> raise (Arg.Bad ("-E: unknown option: " ^ s))
      | [] -> ()
    in aux (list_of_opt s)


  let update_call_sites s =
    let rec aux l = match l with
      | (b, "E")::l -> exported := {!exported with call_sites = b}; aux l
      | (b, "O")::l -> opt := {!opt with call_sites = b}; aux l
      | (b, "all")::l ->
          exported := {!exported with call_sites = b};
          opt := {!opt with call_sites = b};
          aux l
      | (_, s)::_ -> raise (Arg.Bad ("--call-sites: unknown option: " ^ s))
      | [] -> ()
    in aux (list_of_opt s)


  let verbose = ref false
  let set_verbose () = verbose := true

  (* Print name starting with '_' *)
  let underscore = ref false
  let set_underscore () = underscore := true

  let types = ref []
  let update_types s = types := !types @ List.map (fun (_, s) -> s) (list_of_opt ~ignore:["->"] s)

  let internal = ref false
  let set_internal () = internal := true

end


                (********   ATTRIBUTES   ********)

let abspath = ref []                    (* longest paths known *)
let bad_files = ref []

let decs = ref []                       (* all exported value declarations *)
let type_dependencies = ref []          (* like the cmt value_dependencies but for types *)
let references = Hashtbl.create 256     (* all value references *)
let fields = Hashtbl.create 256         (* link from fields (record/variant) paths and locations *)
let corres = Hashtbl.create 256         (* link from dec to def *)

let style = ref []                      (* patterns of type unit which are not () *)
let last_loc = ref Location.none        (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""
let mods = ref []                       (* module path *)


                (********   HELPERS   ********)

let find_path fn = List.find
  (fun path ->
    let lp = String.length path and lf = String.length fn in
      lp >= lf && String.sub path (lp - lf) lf = fn)
  !abspath

let unit fn = try Filename.chop_extension (Filename.basename fn) with _ -> fn

let check_underscore name = not !DeadFlag.underscore || name.[0] <> '_'

let hashtbl_find_list hashtbl key = try Hashtbl.find hashtbl key with Not_found -> []

let exported fn =
  !DeadFlag.exported.print && (!DeadFlag.internal
      || fn.[String.length fn - 1] = 'i'
      || (let src, name = unit !current_src, unit fn in
        String.length name < String.length src
        || String.capitalize_ascii (String.sub name 0 (String.length src)) <> String.capitalize_ascii src)
      || try not (Sys.file_exists (find_path fn ^ "i")) with Not_found -> true)

(* Section printer:
 * section:     `.> SECTION: '
 *              `==========='
 * subsection:  `.>->  SUBSECTION: '
 *              `~~~~~~~~~~~~~~~~~' *)
let section ?(sub = false) title =
  Printf.printf "%s %s:\n%s\n"
    (if sub then ".>-> " else ".>")
    title
    (String.make ((if sub then 5 else 2) + String.length title + 1) (if sub then '~' else '='))

(* End of section *)
let separator () =
  print_endline "Nothing else to report in this section";
  Printf.printf "%s\n\n\n" (String.make 80 '-')

(* Location printer: `filename:line: ' *)
let prloc ?fn (loc : Location.t) = begin match fn with
  | Some s ->
      print_string (Filename.dirname s ^ "/" ^ Filename.basename loc.loc_start.pos_fname)
  | _ -> match find_path loc.loc_start.pos_fname with
    | s -> print_string s
    | exception Not_found -> Printf.printf "!!UNKNOWN<%s>!!%!" loc.loc_start.pos_fname
  end;
  print_char ':';
  print_int loc.loc_start.pos_lnum;
  print_string ": "


                (********   TYPES   *********)

type vd_node =
  {
    loc: Location.t;
    mutable opt_args: string list;
    mutable ptr: vd_node;               (* points to itself if not a binding to another known value *)
  }

let vd_nodes = Hashtbl.create 256

type opt_arg =
  {
    mutable with_val: Location.t list;
    mutable without_val: Location.t list;
  }

let opt_args = ref []


                (********   NODE MANIPULATION   ********)

(* Get or create a vd_node corresponding to the location *)
let vd_node ?(add = false) loc =
  assert (not loc.Location.loc_ghost);
  try (Hashtbl.find vd_nodes loc)
  with Not_found ->
    let rec r = {loc; ptr = r; opt_args = []} in
    if add then
      Hashtbl.add vd_nodes loc r;
    r

(* Locations l1 and l2 are part of a binding from one to another *)
let merge_locs ?add l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then
    (vd_node ?add l1).ptr.ptr <- (vd_node l2).ptr


                (********   PROCESSING   ********)

module DeadType = struct

  let collect_export export path t =

    let save id loc =
      if t.type_manifest = None then
        export path id loc;
      let path = String.concat "." @@ List.rev_map (fun id -> id.Ident.name) (id::path) in
      if Hashtbl.mem fields path then
        Hashtbl.add corres loc
          (let loc = Hashtbl.find fields path in
          loc :: hashtbl_find_list corres loc);
      Hashtbl.replace fields path loc
    in

    match t.type_kind with
      | Type_record (l, _) ->
          List.iter (fun {Types.ld_id; ld_loc; _} -> save ld_id ld_loc) l
      | Type_variant l ->
          List.iter (fun {Types.cd_id; cd_loc; _} -> save cd_id cd_loc) l
      | _ -> ()


  let rec to_string typ = match typ.desc with
    | Tvar i -> begin match i with Some id -> id | None -> "'a" end
    | Tarrow (_, t1, t2, _) ->
        begin match t1.desc with
          | Tarrow _ -> "(" ^ to_string t1 ^ ")"
          | _ -> to_string t1 end
        ^ " -> " ^ to_string t2
    | Ttuple l -> begin match l with
        | e::l ->
            List.fold_left (fun prev typ -> prev ^ " * " ^ to_string typ) (to_string e) l
        | [] -> "*" end
    | Tconstr (path, l, _) -> make_name path l
    | Tobject (self, _) -> "< " ^ to_string self ^ " >"
    | Tfield (s, k, t, t1) ->
        if Btype.field_kind_repr k = Fpresent then
          s ^ ": "
          ^ to_string t
          ^ begin match t1.desc with
              | Tfield _ -> "; " ^ to_string t1
              | _ -> "" end
        else to_string t1
    | Tnil -> "Tnil"
    | Tlink t -> to_string t
    | Tsubst _ -> "Tsubst _"
    | Tvariant {row_more; _} -> to_string row_more
    | Tunivar _ -> "Tunivar _"
    | Tpoly (t, _) -> to_string t
    | Tpackage _ -> "Tpackage _"

  and make_name path l =
    let t = match l with
      | [] -> ""
      | _ -> List.fold_left (fun prev typ -> prev ^ to_string typ ^ " ") "" l;
    in
    let name = Path.name path in
    let len =
      let open Path in
      let rec len path = match path with
        | Pident id when id.Ident.name = "Pervasives" -> String.length "Pervasives."
        | Papply (_, _) | Pident _ -> 0
        | Pdot (path, _, _) -> len path
      in len path
    in
    t ^ String.sub name len (String.length name - len)


  let match_str typ str =
    let typ = to_string typ in
    let str =
      let rec single_space s pos =
        if pos = String.length s - 1 then String.make 1 s.[pos]
        else if s.[pos] = ' ' && s.[pos + 1] = ' ' then single_space s (pos + 1)
        else String.make 1 s.[pos] ^ single_space s (pos + 1)
      in
      single_space str 0
    in

    let rec get_block str pos len =
      if pos = String.length str || str.[pos] = ' ' then
        String.sub str (pos - len) len
      else get_block str (pos + 1) (len + 1)
    in

    let rec compare typ str pos1 pos2 =
      try
        let t = get_block typ pos1 0 in
        let s = get_block str pos2 0 in
        if s <> "" && s <> t then false
        else compare typ str (pos1 + String.length t + 1) (pos2 + String.length s + 1)
      with _ -> (pos1 >= String.length typ) = (pos2 >= String.length str)
    in compare typ str 0 0


  (* Look for bad style typing *)
  let rec check_style t loc = if !DeadFlag.style.opt_arg then match t.desc with
    | Tlink t -> check_style t loc
    | Tarrow (lab, _, t, _) -> begin match lab with
      | Optional lab when check_underscore lab ->
          style := (!current_src, loc, "val f: ... -> (... -> ?_:_ -> ...) -> ...") :: !style
      | _ -> check_style t loc end
    | _ -> ()


  (* declarations Tstr_type *)
  let tstr typ =

    let assoc name loc =
      let path = String.concat "." @@ List.rev @@
        name.Asttypes.txt
        :: typ.typ_name.Asttypes.txt :: !mods
        @ (String.capitalize_ascii (unit !current_src):: [])
      in
      begin try match typ.typ_manifest with
        | Some {ctyp_desc=Ttyp_constr (_, {txt;  _}, _); _} ->
            let loc1 = Hashtbl.find fields
              (String.concat "." @@
                String.capitalize_ascii (unit !current_src)
                :: Longident.flatten txt
                @ (name.Asttypes.txt :: []))
            in
            let loc2 = Hashtbl.find fields path in
            type_dependencies :=
            (loc2, loc1) :: (loc1, loc) :: !type_dependencies;
        | _ -> ()
      with _ -> () end;
      try
        let loc1 = Hashtbl.find fields path in
        type_dependencies := (loc1, loc) :: !type_dependencies
      with Not_found -> Hashtbl.add fields path loc
    in

    match typ.typ_kind with
      | Ttype_record l ->
          List.iter (fun {Typedtree.ld_name; ld_loc; _} -> assoc ld_name ld_loc) l
      | Ttype_variant l ->
          List.iter (fun {Typedtree.cd_name; cd_loc; _} -> assoc cd_name cd_loc) l
      | _ -> ()


  let is_unit t = match (Ctype.repr t).desc with
    | Tconstr (p, [], _) -> Path.same p Predef.path_unit
    | _ -> false

end



module DeadObj = struct

  let references = Hashtbl.create 256                     (* references by path#name *)

  let self_ref = Hashtbl.create 256                       (* references by path#name to itself *)

  let super_ref = Hashtbl.create 256                      (* references by path#name to super *)

  let content = Hashtbl.create 256                        (* classname -> [field names] *)

  let class_dependencies = Hashtbl.create 256             (* inheritance links *)

  let defined = ref []                                    (* all classes defined in the current file *)

  let aliases = ref []                                    (* aliases on class names *)

  let last_class = ref ""                                 (* last class met *)

  let rec sign = function
    | Cty_signature sg -> sg
    | Cty_arrow (_, _, t)
    | Cty_constr (_, _, t) -> sign t


  let rec treat_fields action typ = match typ.desc with
    | Tobject (self, _) -> treat_fields action self
    | Tfield (s, k, _, t) ->
        if Btype.field_kind_repr k = Fpresent then
          action s;
        treat_fields action t
    | Tlink t -> treat_fields action t
    | _ -> ()


  let collect_export export path cltyp loc =


    let str = String.concat "." (List.tl (List.rev_map (fun id -> id.Ident.name) path)) in

    last_class := String.capitalize_ascii (unit !current_src) ^ "." ^ str;
    defined := str :: !defined;
    let save id =
      export path (Ident.create id) loc;
      Hashtbl.replace
        content
        !last_class
        ((false, id) :: hashtbl_find_list content !last_class)
    in

    List.iter
      (fun (p, _) ->
        let name =
          let name = Path.name p in
          if List.mem name !defined then
            String.concat "." (List.rev_map (fun id -> id.Ident.name) (List.tl path)) ^ "." ^ name
          else name
        in
        Hashtbl.replace class_dependencies
          !last_class
          (name :: hashtbl_find_list class_dependencies !last_class))
      (sign cltyp).csig_inher;

    treat_fields save (sign cltyp).csig_self


  let rec make_path ?(hiera = false) e = match e.exp_desc with
    | Texp_send (e, Tmeth_name s, _)
    | Texp_send (e, Tmeth_val {name = s; _}, _) ->
        make_path ~hiera e ^ "#" ^ s
    | Texp_ident (path, _, typ) -> let rec name t = match t.desc with
        | Tlink t -> name t
        | Tvar i -> begin match i with Some id -> id | None -> "'a" end
        | Tconstr (path, _, _) -> Path.name path
        | _ ->
            if hiera then match typ.val_kind with
              | Val_self _ -> "self"
              | Val_anc _ -> "super"
              | _ -> ""
            else
              try List.assoc (Path.name path) !aliases with Not_found -> ""
        in name typ.val_type
    | _ -> ""

  let tstr ({ci_id_class_type=name; ci_expr; _}, _) =
    let name =
      String.capitalize_ascii (unit !current_src) ^ "."
      ^ String.concat "." (List.rev !mods)
      ^ (if !mods <> [] then "." ^ name.Ident.name else name.Ident.name)
    in
    last_class := name;
    if Hashtbl.mem content name then
      let rec structure cl_exp = match cl_exp.cl_desc with
        | Tcl_fun (_, _, _, cl_exp, _)
        | Tcl_apply (cl_exp, _)
        | Tcl_let (_, _, _, cl_exp)
        | Tcl_constraint (cl_exp, _, _, _, _) -> structure cl_exp
        | Tcl_structure cl_struct ->
            let cut_pat s = "self" ^ String.sub s 7 (String.length s - 7) in
            begin match cl_struct.cstr_self with
              | {pat_desc=Tpat_alias ({pat_desc=Tpat_alias ({pat_desc=Tpat_var (id, _); _}, id2, _); _}, id3, _); _} ->
                  aliases := (id.Ident.name, name) :: (cut_pat id2.Ident.name, name) :: (cut_pat id3.Ident.name, name) :: !aliases
              | {pat_desc=Tpat_alias ({pat_desc=Tpat_alias (_, id1, _); _}, id2, _); } ->
                  aliases := (cut_pat id1.Ident.name, name) :: (cut_pat id2.Ident.name, name) :: !aliases
              | _ -> () end;
            List.iter
              (fun f -> match f.cf_desc with
                | Tcf_inherit (_, cl_exp, s, _, l) ->
                    begin match s with
                      | Some s -> let rec name cl_exp = match cl_exp.cl_desc with
                          | Tcl_ident (path, _, _) -> Path.name path
                          | Tcl_fun (_, _, _, cl_exp, _)
                          | Tcl_apply (cl_exp, _)
                          | Tcl_let (_, _, _, cl_exp)
                          | Tcl_constraint (cl_exp, _, _, _, _) -> name cl_exp
                          | _ -> ""
                          in
                          aliases := (s, name cl_exp) :: !aliases
                      | None -> () end;
                    List.iter
                      (fun (s, _) ->
                        Hashtbl.replace content name
                          (List.map
                            (fun (overr, f) -> if f = s then (false, f) else (overr, f))
                            (hashtbl_find_list content name)))
                      l
                | Tcf_method ({txt; _}, _, _) ->
                    Hashtbl.replace content name
                      (List.map
                        (fun (overr, f) -> if f = txt then (true, f) else (overr, f))
                        (hashtbl_find_list content name))
                | _ -> ())
              cl_struct.cstr_fields
        | _ -> ()
      in
      structure ci_expr

  let rec arg typ args = match typ.desc with
    | Tlink t -> arg t args
    | Tarrow (_, t, typ, _) -> if args <> [] then begin arg t [(List.hd args)]; arg typ (List.tl args) end
    | Tobject _ ->
        treat_fields
          (fun s ->
            let _, e, _ = List.hd args in
            begin match e with
            Some e ->
              let path = make_path e ^ "#" ^ s in
              let rec cut_sharp s pos len =
                if len = String.length s then s
                else if s.[pos] = '#' then String.sub s (pos - len) len
                else cut_sharp s (pos + 1) (len + 1)
              in
              let path =
                if List.mem (cut_sharp path 0 0) !defined then
                  String.capitalize_ascii (unit !current_src)
                  ^ "." ^ path
                else path
              in
              if path.[0] <> '#' && exported path then begin
                Hashtbl.add references path (e.exp_loc :: hashtbl_find_list references path);
                let hiera =  make_path ~hiera:true e in
                if String.sub hiera 0 4 = "self" then
                  Hashtbl.add self_ref path (e.exp_loc :: hashtbl_find_list self_ref path)
                else if String.sub hiera 0 5 = "super" then
                  Hashtbl.add super_ref (!last_class ^ "#" ^ s) (path :: hashtbl_find_list super_ref path)
              end
            | None -> () end
          )
          typ
    | _ -> ()

end



module DeadArg = struct

  (* Verify the optional args calls. Treat args *)
  let rec process val_loc args =
    List.iter                               (* treat each arg's expression before all (even if ghost) *)
      (function
        | (_, None, _) -> ()
        | (_, Some e, _) -> check e)
      args;
    if val_loc.Location.loc_ghost then ()   (* Ghostbuster *)
    else begin                              (* else: `begin ... end' for aesthetics *)
      let loc = vd_node val_loc in
      let tbl = Hashtbl.create 256 in       (* tbl: label -> nb of occurences *)

      let treat lab expr =
        let has_val = match expr.exp_desc with
          | Texp_construct (_, {cstr_name="None"; _}, _) -> false
          | _ -> true
        in
        let occur = ref @@
          try Hashtbl.find tbl lab + 1
          with Not_found -> Hashtbl.add tbl lab 1; 1
        in
        let count x l = List.length @@ List.find_all (( = ) x) l in
        let rec locate loc =
          let count = if loc == loc.ptr then 0 else count lab loc.opt_args in
          if loc == loc.ptr || count >= !occur then loc.loc
          else (occur := !occur - count; locate loc.ptr)
        in
        if check_underscore lab then
          opt_args := (locate loc, lab, has_val,
              if expr.exp_loc.Location.loc_ghost then !last_loc
              else expr.exp_loc)
            :: !opt_args
      in

      List.iter
        (function
          | (Asttypes.Optional lab, Some expr, _) ->
            treat lab expr
          | _ -> ())
        args
    end


  (* Verify the nature of the argument to detect and treat function applications and uses *)
  and check e =
    (* Optional arguments used to match a signature are considered used *)
    let rec get_sig_args args typ = match typ.desc with
      | Tarrow (Asttypes.Optional _ as arg, _, t, _) ->
          get_sig_args ((arg, Some {e with exp_desc=Texp_constant (Asttypes.Const_int 0)}, Optional)::args) t
      | Tarrow (_, _, t, _)
      | Tlink t -> get_sig_args args t
      | _ -> args

    in match e.exp_desc with

      | Texp_apply ({exp_desc=Texp_ident (_, _, {val_loc; _}); _}, args)
      | Texp_apply ({exp_desc=Texp_field (_, _, {lbl_loc=val_loc; _}); _}, args) ->
          process val_loc (get_sig_args args e.exp_type);
          if not val_loc.Location.loc_ghost then
            last_loc := val_loc

      | Texp_ident (_, _, {val_loc; _}) ->
          process val_loc (get_sig_args [] e.exp_type)

      | Texp_let (* Partial application as argument may cut in two parts:
                  * let _ = partial in implicit opt_args elimination *)
          ( _,
            [{vb_expr={exp_desc=Texp_apply ({exp_desc=Texp_ident (_, _, {val_loc; _}); _}, _); _}; _}],
            { exp_desc=Texp_function (_,
                [{c_lhs={pat_desc=Tpat_var (_, _); pat_loc={loc_ghost=true; _}; _};
                  c_rhs={exp_desc=Texp_apply (_, args); exp_loc={loc_ghost=true; _}; _}; _}],_);
              exp_loc={loc_ghost=true; _};_}) ->
          process val_loc args

        | _ -> ()


  (* Construct the 'opt_args' list of func in node *)
  let rec node_build node expr = match expr.exp_desc with
    | Texp_function (lab, [{c_lhs={pat_type; _}; c_rhs=exp; _}], _) ->
        DeadType.check_style pat_type expr.exp_loc;
        begin match lab with
          | Asttypes.Optional s ->
              node.opt_args <- s::node.opt_args;
              node_build node exp
          | _ -> () end
    | Texp_apply ({exp_desc=Texp_ident (_, _, {val_loc=loc2; _}); _}, args)
    | Texp_apply ({exp_desc=Texp_field (_, _, {lbl_loc=loc2; _}); _}, args) ->
        process loc2 args;
        merge_locs node.loc loc2
    | Texp_ident (_, _, {val_loc=loc2; _}) ->
        merge_locs node.loc loc2
    | _ -> ()

end



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
                Hashtbl.add references lab.lbl_loc (p.pat_loc :: hashtbl_find_list references lab.lbl_loc))
            l
      | _ -> () end;
    super.pat self p
  in

  let expr self e = begin match e.exp_desc with   (* most of the processing starts here *)
    | Texp_apply (exp, args) ->
        if DeadFlag.(!opt.always || !opt.never) then treat_exp exp args;
        begin match exp.exp_desc with
          | Texp_ident (_, {loc; _}, _)
          when loc.Location.loc_start = exp.exp_loc.Location.loc_start -> ()
              (* The application is due to dispatching, wich is a case we do not want to treat otherwise
               * all object's content would be marked as used at this point... *)
          | Texp_ident (_, _, {val_type; _}) ->
              DeadObj.arg val_type args
          | _ -> DeadObj.arg exp.exp_type args end

    | Texp_ident (_, _, {Types.val_loc=loc; _})
    | Texp_field (_, _, {lbl_loc=loc; _})
    | Texp_construct (_, {cstr_loc=loc; _}, _)
      when not loc.Location.loc_ghost && exported loc.Location.loc_start.pos_fname ->
        Hashtbl.add references loc (e.exp_loc :: hashtbl_find_list references loc)

    | Texp_send _ ->
        begin try
          let path = DeadObj.make_path e in
          let rec cut_sharp s pos len =
            if len = String.length s then s
            else if s.[pos] = '#' then String.sub s (pos - len) len
            else cut_sharp s (pos + 1) (len + 1)
          in
          let path =
            if List.mem (cut_sharp path 0 0) !DeadObj.defined then
              String.capitalize_ascii (unit !current_src)
              ^ "." ^ path
            else path
          in
          if path.[0] <> '#' && exported path then begin
            Hashtbl.add DeadObj.references path (e.exp_loc :: hashtbl_find_list DeadObj.references path);
            let hiera =  DeadObj.make_path ~hiera:true e in
            if String.sub hiera 0 4 = "self" then
              Hashtbl.add DeadObj.self_ref path (e.exp_loc :: hashtbl_find_list DeadObj.self_ref path)
            else if String.sub hiera 0 5 = "super" then begin
              let s =
                let tmp = cut_sharp path 0 0 in
                String.sub path (String.length tmp + 1) (String.length path - String.length tmp - 1)
              in
              Hashtbl.add DeadObj.super_ref (!DeadObj.last_class ^ "#" ^ s) (path :: hashtbl_find_list DeadObj.super_ref path)
            end
          end
        with _ -> () end

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
  {super with structure_item; expr; pat}



let rec collect_export path u signature =

  let export ?(sep = ".") path id loc =
    if not loc.Location.loc_ghost && u = unit loc.Location.loc_start.Lexing.pos_fname
    && check_underscore id.Ident.name then
      decs := (!current_src,
          String.concat "." (List.rev_map Ident.name path)
          ^ sep
          ^ id.Ident.name,
          loc)
        :: !decs;
  in
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
          export path id val_loc;
    | Sig_type (id, t, _) ->
          DeadType.collect_export (export ~sep:".") (id::path) t
    | Sig_class (id, {Types.cty_type = t; cty_loc = loc; _}, _) -> DeadObj.collect_export (export ~sep:"#") (id::path) t loc
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
          Hashtbl.add references vd1 (vd2 ::hashtbl_find_list references vd1)
        else if not (is_implem fn1 && has_iface fn1) then begin
          Hashtbl.add corres vd1 (vd2 :: hashtbl_find_list corres vd1);
          Hashtbl.add references vd1 @@ List.sort_uniq compare
            ((hashtbl_find_list references vd1)
            @ hashtbl_find_list references vd2)
        end
        else begin
          Hashtbl.add corres vd2 (vd1 :: hashtbl_find_list corres vd2);
          Hashtbl.add references vd2 @@ List.sort_uniq compare
            ((hashtbl_find_list references vd2)
            @ hashtbl_find_list references vd1)
        end
      in

      begin match cmt with
        | Some {cmt_annots=Implementation x; cmt_value_dependencies; _} ->
            ignore (collect_references.structure collect_references x);
            if !DeadFlag.exported.print then begin
              List.iter assoc (List.rev_map (fun (vd1, vd2) -> (vd1.Types.val_loc, vd2.Types.val_loc)) cmt_value_dependencies);
              List.iter assoc !type_dependencies
            end;
            type_dependencies := [];
            DeadObj.aliases := [];
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
  Hashtbl.iter
    (fun a l ->
      let met = ref [] in
      List.iter
        (fun c -> List.iter
          (fun (_, f) ->
            let overr, _ = List.find (fun (_, s) -> s = f) (hashtbl_find_list DeadObj.content a) in
            if not (overr || List.mem f !met) then begin
              met := f :: !met;
              Hashtbl.replace
                DeadObj.references
                (c ^ "#" ^ f)
                (List.sort_uniq compare (hashtbl_find_list DeadObj.references (c ^ "#" ^ f) @ hashtbl_find_list DeadObj.references (a ^ "#" ^ f)));
              Hashtbl.replace
                DeadObj.references
                (a ^ "#" ^ f)
                (List.sort_uniq compare (hashtbl_find_list DeadObj.self_ref (c ^ "#" ^ f) @ hashtbl_find_list DeadObj.references (a ^ "#" ^ f)));
            end)
          (hashtbl_find_list DeadObj.content c))
        (List.rev l))
    DeadObj.class_dependencies;
  Hashtbl.iter
    (fun key l ->
      List.iter
        (fun path -> if Hashtbl.mem DeadObj.self_ref path then
          Hashtbl.replace
            DeadObj.references
            key
            (List.sort_uniq compare (hashtbl_find_list DeadObj.self_ref path @ hashtbl_find_list DeadObj.references key))
        )
        l)
    DeadObj.super_ref;
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
