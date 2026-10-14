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

let dependencies = ref []   (* like the cmt value_dependencies but for types *)



                (********   HELPERS   ********)

let is_unit t = match get_deep_desc t with
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


let add_component ~type_loc ~type_path ~cf_loc ~cf_name state =
  let cf_path = String.concat "." [type_path; cf_name] in
  state
  |> State.add_loc_binding
      ~elt_kind:`Type ~elt_path:type_path ~elt_loc:type_loc
  |> State.add_loc_binding
      ~elt_kind:`Ctor_field ~elt_path:cf_path ~elt_loc:cf_loc
  |> State.add_type_component ~type_loc ~cf_name ~cf_loc


let fold_components ~f type_decl state =
  match type_decl.type_kind with
    | Type_record (fields, _) ->
        let f state {Types.ld_id; ld_loc; _} =
          f ld_id ld_loc state
        in
        List.fold_left f state fields
    | Type_variant (ctors, _) ->
        let f state {Types.cd_id; cd_loc; _} =
          f cd_id cd_loc state
        in
        List.fold_left f state ctors
    | _ -> state

                (********   PROCESSING  ********)

let collect_export path t =
  let state = State.get_current () in
  let type_loc = t.type_loc.Location.loc_start in
  let type_path = List.rev path |> String.concat "." in
  let add_declaration cf_id cf_loc state =
    let cf_name = Ident.name cf_id in
    if DeadCommon.should_export cf_loc cf_name
      && t.type_manifest = None
      (* do not export t1 when there is an explicit equation t1 = t2 *)
    then
      let cf_path = String.concat "." [type_path; cf_name] in
      let cf_loc = cf_loc.Location.loc_start in
      state
      |> State.add_exported_declaration
          ~elt_kind:`Ctor_field ~elt_loc:cf_loc ~elt_path:cf_path
      |> add_component ~type_loc ~type_path ~cf_loc ~cf_name
    else state
  in
  let state = fold_components ~f:add_declaration t state in
  let _apply_lexifi =
    match t.type_kind with
      | Type_record (fields, _) ->
          List.iter
            (fun {Types.ld_loc; ld_type; _} ->
              !DeadLexiFi.export_type ld_loc.Location.loc_start (to_string ld_type)
            )
            fields
      | _ -> ()
  in
  State.update state

let correct_export t =
  let unexport loc state =
    let elt_loc = loc.Location.loc_start in
    State.remove_exported_declaration ~elt_kind:`Ctor_field ~elt_loc state
  in
  State.get_current ()
  |> fold_components ~f:(fun _id -> unexport) t
  |> State.update


let collect_references cf_loc use_loc =
  let state = State.get_current() in
  State.add_use ~elt_kind:`Ctor_field ~elt_loc:cf_loc ~use_loc state
  |> State.update


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


let collect_eq_from_typ_decl ~t1_path ~t2_path type_decl =
  let state = State.get_current() in
  let state = State.add_type_equivalence ~t1_path ~t2_path state in
  let type_loc = type_decl.type_loc.Location.loc_start in
  let add_component cf_id cf_loc state =
    let cf_loc = cf_loc.Location.loc_start in
    let cf_name = Ident.name cf_id in
    let cf_path = t1_path ^ "." ^ cf_name in
    match State.find_loc ~elt_kind:`Ctor_field ~elt_path:cf_path state with
    | None ->
        add_component ~type_loc ~type_path:t1_path ~cf_loc ~cf_name state
    | _ -> state
  in
  fold_components ~f:add_component type_decl state
  |> State.update


let find_longest_known_type_path rev_type_path =
  let state = State.get_current() in
  let rec find_longest_known_path = function
    | [] -> assert false (* There must be at least one element *)
    | type_path :: [] -> (* external module *)
        type_path
    | local_type_path :: rev_internal_path as rev_type_path ->
        let type_path = List.rev rev_type_path |> String.concat "." in
        match State.find_loc ~elt_kind:`Type ~elt_path:type_path state with
        | Some _ -> (* known path *)
            type_path
        | None -> (* path unknown; remove the latest module in the path *)
            let reduced_path =
              match rev_internal_path with
              | [] | _::[] -> local_type_path :: []
              | _::rev_internal_path -> local_type_path::rev_internal_path
            in
            find_longest_known_path reduced_path
  in
  find_longest_known_path rev_type_path


let find_longest_known_path ~mod_path type_path =
  match mod_path with
  | [] -> assert false
  | mod_path::rev_internal_path ->
      let local_type_path = String.concat "." [mod_path; type_path] in
      find_longest_known_type_path (local_type_path :: rev_internal_path)

let collect_eq_from_module_alias
    ~rev_alias_path ~original_path ~sub_path type_decl
=
  let local_type_path = String.concat "." sub_path in
  let t1_path =
    List.rev (local_type_path::rev_alias_path)
    |> String.concat "."
  in
  let t2_path =
    let mod_path =
      Utils.normalize_mod_path ~rev_curr_path:(List.tl rev_alias_path) original_path
    in
    find_longest_known_path ~mod_path local_type_path
  in
  collect_eq_from_typ_decl ~t1_path ~t2_path type_decl


let collect_eq_from_include ~incl_path ~path type_decl =
  let state = State.get_current () in
  let module_id = State.File_infos.get_modname state.file_infos in
  (* internal path *)
  let rev_curr_path = !DeadCommon.mods @ [module_id] in
  let local_type_path = String.concat "." path in
  let t1_path =
    List.rev (local_type_path::rev_curr_path)
    |> String.concat "."
  in
  let t2_path =
    let mod_path = Utils.normalize_mod_path ~rev_curr_path incl_path in
    find_longest_known_path ~mod_path local_type_path
  in
  collect_eq_from_typ_decl ~t1_path ~t2_path type_decl


let tstr typ =
  let state = State.get_current() in
  let modname = State.File_infos.get_modname state.file_infos in
  let rev_curr_path = !DeadCommon.mods @ [modname] in
  let t1_path =
    List.rev (typ.typ_name.Asttypes.txt :: rev_curr_path)
    |> String.concat "."
  in
  let t1_loc = typ.typ_loc.Location.loc_start in

  (* A type equation [type t1 = t2 = ...] produces a
     [typ_manifest = Some (Ttyp_constr t2)] in t1
     In this situation, we want to remember the equivalence between t1 and t2
     for later resolution of equivalence classes and merging all their
     references (see {!prepare_report} below).
  *)
  let state =
    match typ.typ_manifest with
    | Some {ctyp_desc=Ttyp_constr (original_path, _, _); _} ->
        let t2_path =
          Utils.normalize_mod_path ~rev_curr_path original_path
          |> find_longest_known_type_path
        in
        State.add_type_equivalence ~t1_path ~t2_path state
    | _ -> state
  in

  let assoc cf_name cf_loc state =
    (* store the association from name to loc *)
    let cf_name = cf_name.Asttypes.txt in
    let cf_loc = cf_loc.Location.loc_start in
    let cf_path = String.concat "." [t1_path; cf_name] in
    match State.find_loc ~elt_kind:`Ctor_field ~elt_path:cf_path state with
    | None ->
        add_component ~type_loc:t1_loc ~type_path:t1_path ~cf_loc ~cf_name state
    | Some known_loc when known_loc <> cf_loc ->
        (* The path is known because the current compilation unit exports it *)
        (* store dependency between .ml and .mli *)
        dependencies := (known_loc, cf_loc) :: !dependencies;
        state
    | _ -> state
  in
  let state =
    match typ.typ_kind with
      | Ttype_record fields ->
          let assoc state {Typedtree.ld_name; ld_loc; _} =
            assoc ld_name ld_loc state
          in
          List.fold_left assoc state fields
      | Ttype_variant ctors ->
          let assoc state {Typedtree.cd_name; cd_loc; _} =
            assoc cd_name cd_loc state
          in
          List.fold_left assoc state ctors
      | _ -> state
  in
  let _apply_lexifi =
    match typ.typ_kind with
      | Ttype_record fields ->
          List.iter
            (fun {Typedtree.ld_type; _} ->
              !DeadLexiFi.tstr_type typ (to_string ld_type.ctyp_type)
            )
            fields
      | _ -> ()
  in
  State.update state


let prepare_report () =
  let state = State.get_current () in
  State.Ctors_fields.resolve_equivalences state.ctors_fields
  |> ignore


let report () =
  let state = State.get_current () in
  Report.report ~elt_kind:`Ctor_field state


                (********   WRAPPING  ********)

let wrap f x =
  let state = State.get_current () in
  if Config.must_report_section state.config.sections.types then
    f x
  else ()

let collect_export path t = wrap (collect_export path) t
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
