(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2025 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

open Asttypes
open Types
open Typedtree

open DeadCommon



                (********   ATTRIBUTES  ********)

let decs = Hashtbl.create 256

let dependencies = ref []   (* like the cmt value_dependencies but for types *)

let equivalences = ref []   (* t1 = t2 *)



                (********   HELPERS   ********)

let is_unit t = match get_desc t with
  | Tconstr (p, [], _) -> Path.same p Predef.path_unit
  | _ -> false


let nb_args ~keep typ =
  let rec loop n = function
    | Tarrow (_, _, typ, _) when keep = `All -> loop (n + 1) (get_desc typ)
    | Tarrow (Labelled _, _, typ, _) when keep = `Lbl -> loop (n + 1) (get_desc typ)
    | Tarrow (Optional _, _, typ, _) when keep = `Opt -> loop (n + 1) (get_desc typ)
    | Tarrow (Nolabel, _, typ, _) when keep = `Reg -> loop (n + 1) (get_desc typ)
    | Tarrow (_, _, typ, _) -> loop n (get_desc typ)
    | _ -> n
  in
  loop 0 (get_desc typ)


let to_string typ =
  Printtyp.type_expr Format.str_formatter typ;
  Format.flush_str_formatter ()


let is_type s =
  let rec blk s p l acc =
    try
      if s.[p] = '.' then
        let acc = String.sub s (p - l) l :: acc in
        blk s (p + 1) 0 acc
      else blk s (p + 1) (l + 1) acc
    with _ -> String.sub s (p - l) l :: acc
  in
  if not (String.contains s '.') then false
  else
    match blk s 0 0 [] with
    | hd :: cont :: _ ->
      String.capitalize_ascii hd = hd || String.lowercase_ascii cont = cont
    | _ ->
      assert false



                (********   PROCESSING  ********)

let collect_export path u stock t =

  let stock =
    if stock == DeadCommon.decs then decs
    else stock
  in

  let save id loc =
    let id = Ident.name id in
    if t.type_manifest = None then
      (* do not export t1 when there is an explicit equation t1 = t2 *)
      export path u stock id loc;
    let path = String.concat "." @@ List.rev (id::path) in
    Hashtbl.replace fields path loc.Location.loc_start
  in

  match t.type_kind with
    | Type_record (l, _) ->
        List.iter
          (fun {Types.ld_id; ld_loc; ld_type; _} ->
            save ld_id ld_loc;
            !DeadLexiFi.export_type ld_loc.Location.loc_start (to_string ld_type)
          )
          l
    | Type_variant (l, _) ->
        List.iter (fun {Types.cd_id; cd_loc; _} -> save cd_id cd_loc) l
    | _ -> ()

let correct_export t =
  let unexport loc = DeadCommon.unexport decs loc in
  match t.type_kind with
    | Type_record (l, _) ->
        List.iter
          (fun {Types.ld_loc; _} ->
            unexport ld_loc;
          )
          l
    | Type_variant (l, _) ->
        List.iter (fun {Types.cd_loc; _} -> unexport cd_loc) l
    | _ -> ()


let collect_references loc exp_loc =
  LocHash.add_set references loc exp_loc


(* Look for bad style typing *)
let rec check_style t loc =
  let state = State.get_current() in
  if state.config.sections.style.opt_arg then
    match get_deep_desc t with
      | Tarrow (lab, _, t, _) -> begin
          match lab with
            | Optional lab when check_underscore lab ->
                let builddir = State.File_infos.get_builddir state.file_infos in
                let fn = Filename.concat builddir loc.Lexing.pos_fname in
                style :=
                  (fn, loc,
                   "val f: ... -> (... -> ?_:_ -> ...) -> ...")
                  :: !style
            | _ -> check_style t loc end
      | _ -> ()


let add_type_eq component_path eq_type_path component_name =
  (* Store t1 = t2 equivalence, with t2 assumed to be defined outside the
     current compilation unit *)
  let eq_path = eq_type_path ^ "." ^ component_name in
  equivalences := (component_path, eq_path) :: !equivalences


let rec add_type_eq_internal ~internal_path ~component_path ~component_name =
  match internal_path with
  | [] -> assert false (* There must be at least one element *)
  | external_type_path :: [] -> (* external alias *)
      add_type_eq component_path external_type_path component_name
  | external_type_path :: rev_internal_path ->
      let eq_type_path =
        List.rev internal_path |> String.concat "."
      in
      let eq_component_path = eq_type_path ^ "." ^ component_name in
      match Hashtbl.find_opt fields eq_component_path with
      | Some _ -> (* internal alias *)
          add_type_eq component_path eq_type_path component_name
      | None ->
          let internal_path =
            match rev_internal_path with
            | [] | _::[] -> external_type_path :: []
            | _::rev_internal_path -> external_type_path :: rev_internal_path
          in
          add_type_eq_internal ~internal_path ~component_path ~component_name


let normalize_mod_path rev_current_path mod_path =
  let sub_path = Path.name mod_path in
  if Path.head mod_path |> Ident.global then
    (* External mod_path. Keep as is *)
    sub_path :: []
  else
    (* Internal mod_path *)
    sub_path :: rev_current_path


let collect_eq_from_module_alias
    ~rev_alias_path ~original_path ~sub_path type_decl
=
  let type_path =
    List.rev_append rev_alias_path sub_path
    |> String.concat "."
  in
  let internal_path =
    let normalized_original_path =
      normalize_mod_path (List.tl rev_alias_path) original_path
    in
    match normalized_original_path with
    | original_path :: rev_path ->
        let original_type_path = String.concat "." (original_path :: sub_path) in
        original_type_path :: rev_path
    | [] -> assert false
  in
  let add_type_eq loc component_id =
    let component_name = Ident.name component_id in
    let component_path = type_path ^ "." ^ component_name in
    if not (Hashtbl.mem fields component_path) then
      Hashtbl.add fields component_path loc.Location.loc_start;
    add_type_eq_internal ~internal_path ~component_path ~component_name
  in
  match type_decl.type_kind with
    | Type_record (l, _) ->
        List.iter (fun {Types.ld_id; ld_loc; _} -> add_type_eq ld_loc ld_id) l
    | Type_variant (l, _) ->
        List.iter (fun {Types.cd_id; cd_loc; _} -> add_type_eq cd_loc cd_id) l
    | _ -> ()


let collect_eq_from_include ~incl_path ~path type_decl =
  let state = State.get_current () in
  let module_id = State.File_infos.get_modname state.file_infos in
  (* internal path *)
  let rev_curr_path = !DeadCommon.mods @ [module_id] in
  let type_path =
    List.rev_append rev_curr_path path
    |> String.concat "."
  in
  let internal_path =
    let normalized_incl_path =
      normalize_mod_path rev_curr_path incl_path
    in
    match normalized_incl_path with
    | incl_path :: rev_path ->
        let incl_type_path = String.concat "." (incl_path :: path) in
        incl_type_path :: rev_path
    | [] -> assert false
  in
  let add_type_eq component_id =
    let component_name = Ident.name component_id in
    (* internal path *)
    let component_path = type_path ^ "." ^ component_name in
    add_type_eq_internal ~internal_path ~component_path ~component_name
  in
  match type_decl.type_kind with
    | Type_record (l, _) ->
        List.iter (fun {Types.ld_id; _} -> add_type_eq ld_id) l
    | Type_variant (l, _) ->
        List.iter (fun {Types.cd_id; _} -> add_type_eq cd_id) l
    | _ -> ()

let tstr typ =
  let state = State.get_current() in
  let modname = State.File_infos.get_modname state.file_infos in

  (* A type equation [type t1 = t2 = ...] produces a
     [typ_manifest = Some (Ttyp_constr t2)] in t1
     In this situation, we want to remember the equality between t1 and t2's
     components, for later resolution of equivalence classes and merging
     all their references (see {!prepare_report} below).
  *)
  let eq_type_path =
    match typ.typ_manifest with
    | Some {ctyp_desc=Ttyp_constr (_, {txt;  _}, _); _} ->
        let path = String.concat "." (Longident.flatten txt) in
        Some path
    | _ -> None
  in

  let handle_external_type_eq : string -> string -> unit =
    match eq_type_path with
    | None -> fun _ _ -> ()
    | Some eq_type_path ->
        fun component_path component_name ->
          add_type_eq component_path eq_type_path component_name
  in

  let handle_internal_type_eq : string -> string -> unit =
    (* Store t1 = t2 equivalence as a dependency, with t2 defined within the
       current compilation unit *)
    match eq_type_path with
    | None -> fun _ _ -> ()
    | Some eq_type_path ->
        fun component_path component_name ->
          let eq_type_path = String.concat "." [modname; eq_type_path] in
          let eq_component_path =
            String.concat "." [eq_type_path; component_name]
          in
          match Hashtbl.find_opt fields eq_component_path with
          | None -> () (* t2 is not defined locally *)
          | Some _ ->
              add_type_eq component_path eq_type_path component_name
  in

  let handle_type_dep loc path_loc component_path component_name =
    handle_internal_type_eq component_path component_name;
    if path_loc <> loc then
      (* store dependency between .ml and .mli *)
      dependencies := (path_loc, loc) :: !dependencies
  in

  let assoc name loc =
    (* store the association from name to loc in fields,
       the dependenicies and the equivalences *)
    let component_name = name.Asttypes.txt in
    let path =
      let partial_path_rev =
        component_name :: typ.typ_name.Asttypes.txt :: !mods
      in
      modname :: List.rev partial_path_rev
      |> String.concat "."
    in
    handle_external_type_eq path component_name;
    match Hashtbl.find_opt fields path with
    | None ->
        Hashtbl.add fields path loc;
        handle_internal_type_eq path component_name
    | Some path_loc ->
        (* The path is known because the current compilation unit exports it *)
        handle_type_dep loc path_loc path component_name
  in
  let assoc name loc ctyp =
    assoc name loc;
    !DeadLexiFi.tstr_type typ ctyp
  in

  match typ.typ_kind with
    | Ttype_record l ->
        List.iter
          (fun {Typedtree.ld_name; ld_loc; ld_type; _} ->
            assoc ld_name ld_loc.Location.loc_start (to_string ld_type.ctyp_type)
          )
          l
    | Ttype_variant l ->
        List.iter
          (fun {Typedtree.cd_name; cd_loc; _} -> assoc cd_name cd_loc.Location.loc_start _variant)
          l
    | _ -> ()


let prepare_report () =
  (* implement a pseudo union-find via 2 tables : references and reprs *)
  (* references hold merged references of a union class with the
     representative as key.*)
  let references = LocHash.create 128 in
  (* reprs points to another member of the location's equivalence class.
     This memeber was the representative at some point. There are no
     circular references.
     _The_ representative of a class points to itself.
     Use get_repr to get _the_ representative of a location's class.
  *)
  let reprs = Hashtbl.create 128 in
  let init_refs loc =
    (* the initial value for a single-element class is the set of references
       gathered during the analysis *)
    LocHash.find_set DeadCommon.references loc
    |> LocHash.replace references loc
  in
  let rec get_repr loc =
    (* explore members of loc's class until finding the class representative *)
    match Hashtbl.find_opt reprs loc with
    | None ->
        (* loc does not belong to a class yet. Setup its own *)
        init_refs loc;
        Hashtbl.add reprs loc loc;
        loc
    | Some repr when repr = loc -> loc (* class representative found *)
    | Some repr ->  get_repr repr (* class member but not the representative *)
  in
  let merge_references (path1, path2) =
    let loc1 = Hashtbl.find_opt fields path1 in
    let loc2 = Hashtbl.find_opt fields path2 in
    match loc1, loc2 with
    | None, _ | _, None -> ()
    | Some loc1, Some loc2 ->
        let repr1 = get_repr loc1 in
        let repr2 = get_repr loc2 in
        Hashtbl.replace reprs repr1 repr2;
        (* repr1 is now represented by repr2: its references are transfered *)
        LocHash.merge_set references repr2 references repr1;
        LocHash.remove references repr1
  in
  let update_references loc =
    Option.iter
      (fun loc ->
        let repr = get_repr loc in
        let refs = LocHash.find_set references repr in
        (* refs include the references gathered for loc and all the members
           of its equivalence class *)
        LocHash.replace DeadCommon.references loc refs
      )
      loc
  in
  let update_references (path1, path2) =
    Hashtbl.find_opt fields path1 |> update_references;
    Hashtbl.find_opt fields path2 |> update_references
  in
  List.iter merge_references !equivalences;
  List.iter update_references !equivalences


let report () =
  let state = State.get_current () in
  report_basic
    decs
    "UNUSED CONSTRUCTORS/RECORD FIELDS"
    state.config.sections.types


                (********   WRAPPING  ********)

let wrap f x =
  let state = State.get_current () in
  if Config.must_report_section state.config.sections.types then
    f x
  else ()

let collect_export path u stock t = wrap (collect_export path u stock) t
let tstr typ = wrap tstr typ
let prepare_report () = wrap prepare_report ()
let report () = wrap report ()

let collect_eq_from_module_alias
    ~rev_alias_path ~original_path ~sub_path type_decl
=
  wrap
    (collect_eq_from_module_alias
      ~rev_alias_path ~original_path ~sub_path
    )
    type_decl

let collect_eq_from_include ~incl_path ~path type_decl =
  wrap (collect_eq_from_include ~incl_path ~path) type_decl
