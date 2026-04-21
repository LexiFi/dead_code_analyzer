(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2025 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)


module Default = struct
  let sig_value _ = ()
  let export_type _ _ = ()
  let type_ext _ = ()
  let type_decl _ = ()
  let tstr_type _ _ = ()
  let ttype_of _ = ()
  let prepare_report _ = ()
end

let sig_value = ref Default.sig_value
let export_type = ref Default.export_type
let type_ext = ref Default.type_ext
let type_decl = ref Default.type_decl
let tstr_type = ref Default.tstr_type
let ttype_of = ref Default.ttype_of
let prepare_report = ref Default.prepare_report


(** Extensions used internally at LexiFi. *)
module Extension = struct

  open Parsetree
  open Types
  open Typedtree

  open DeadCommon

                (********   ATTRIBUTES   ********)

  let dyn_rec = ref [] (* Record names used for dynamic typing and locations of those uses *)
  let str = Hashtbl.create 256
  let used = ref []

  let field_link = Hashtbl.create 256
  let dyn_used = Hashtbl.create 256

  let sig_value (value : Types.value_description) =
    let add strct = match strct.pstr_desc with
      | Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _}, _) ->
          hashtbl_add_unique_to_list str s value.val_loc.loc_start
      | _ -> ()
    in
    let add = function
      | {attr_name = {txt = "mlfi.value_approx"; _}; attr_payload = PStr structure; _} ->
          List.iter add structure
      | _ -> ()
    in
    List.iter add value.val_attributes

  let type_ext ct =
    (* TO CHECK *)
    List.iter
      (fun {attr_name = {txt; _}; _} ->
        used := (txt, ct.ctyp_loc.loc_start) :: !used;
      )
      ct.ctyp_attributes

  let type_decl td =
     List.iter
       (fun {attr_name = {txt; _}; _} ->
          used := (txt, td.typ_loc.loc_start) :: !used;
       )
       td.typ_type.type_attributes

  let tstr_type typ ctype =
    let state = State.get_current () in
    let modname = State.File_infos.get_modname state.file_infos in
    let path =
      let partial_path_rev =
        typ.typ_name.Asttypes.txt :: !mods
      in
      modname :: List.rev partial_path_rev
      |> String.concat "."
    in
    let is_user_defined s =
      let l = [_variant; "bool"; "float"; "int"; "string"; "unit"] in
      let mod_name =
        let rec loop s pos len =
          if len = String.length s then s
          else if s.[pos] = '.' then String.sub s (pos - len) len
          else loop s (pos + 1) (len + 1)
        in loop s 0 0
      in
      not (String.contains s ' ')
      && (s <> String.capitalize_ascii s && not (List.mem s l)
        || String.contains s '.' && mod_name <> "Pervasives")
    in
    if is_user_defined ctype then
      hashtbl_add_to_list field_link path ctype

  let ttype_of e =
    let state = State.get_current () in
    let modname = State.File_infos.get_modname state.file_infos in
    let name =
      List.rev (modname :: !mods)
      |> String.concat "."
    in
    let call_site =
      if e.exp_loc.Location.loc_ghost then !last_loc
      else e.exp_loc.Location.loc_start
    in
    dyn_rec := (name, e.exp_type, call_site) :: !dyn_rec

  let prepare_report decs =
    let state = State.get_current () in
    let sections = state.config.sections in
    List.iter
      (fun (strin, pos) ->
        hashtbl_find_list str strin
        |> List.iter
          (fun loc ->
            if exported sections.exported_values loc then
              LocHash.add_set references loc pos
          )
      )
      !used;
    let rec process (p, typ, call_site) =
      match get_deep_desc typ with
      | Tarrow (_, t, _, _) -> process (p, t, call_site)
      | Ttuple ts -> List.iter (fun t -> process (p, t, call_site)) ts
      | Tconstr (path, ts, _) ->
          let name = Path.name path in
          let name =
            if String.contains name '.' then name
            else p ^ "." ^ name
          in
          let met = ref [] in
          let rec proc name =
            if not (List.mem name !met) then begin
              met := name :: !met;
              name :: List.fold_left (fun acc name -> acc @ (proc name)) [] (hashtbl_find_list field_link name)
            end
            else []
          in
          List.iter
            (fun typ ->
              hashtbl_add_to_list dyn_used typ call_site
            )
            (proc name);
          List.iter (fun t -> process (p, t, call_site)) ts
      | _ -> ()
    in
    List.iter process !dyn_rec;
    Hashtbl.iter
      (fun loc (_, path) ->
        let rec get_type s pos =
          if pos = 0 then s
          else if s.[pos] = '.' then String.sub s 0 pos
          else get_type s (pos - 1)
        in
        List.iter
          ( if exported ~is_type:true sections.types loc then LocHash.add_set references loc
            else ignore
          )
          (hashtbl_find_list dyn_used (get_type path (String.length path - 1)))
      )
      decs

end

let set_hooks () =
  sig_value := Extension.sig_value;
  type_ext := Extension.type_ext;
  type_decl := Extension.type_decl;
  tstr_type := Extension.tstr_type;
  ttype_of := Extension.ttype_of;
  prepare_report := Extension.prepare_report
