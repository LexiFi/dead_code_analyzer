(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Extensions internally used at Lexifi. *)


(*   .^.  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  .^.   *)
(*  / ! \  DO NOT DELETE UNLESS YOU CAN COMPILE WITH `make lexifi' AND YOU KNOW WHAT YOU ARE DOING  / ! \  *)
(* /_____\   /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\   /_____\ *)


open Parsetree
open Types
open Typedtree

open DeadCommon


                (********   ATTRIBUTES   ********)

let dyn_rec = ref []            (* Record names used for dynamic typing and locations of those uses *)
let str = Hashtbl.create 256
let used = ref []

let field_link = Hashtbl.create 256
let dyn_used = Hashtbl.create 256



let () =

  DeadLexiFi.sig_value :=
    (fun value ->
      let add strct = match strct.pstr_desc with
        | Pstr_eval ({pexp_desc=Pexp_constant (PConst_string (s, _)); _}, _) ->
            hashtbl_add_unique_to_list str s value.val_loc.Location.loc_start
        | _ -> ()
      in
      let add = function
        | ({Asttypes.txt="mlfi.value_approx"; _}, PStr structure) ->
            List.iter add structure
        | _ -> ()
      in
      List.iter add value.val_attributes
    );


  DeadLexiFi.type_ext :=
    (fun ct ->
      match ct.ctyp_desc with
      | Ttyp_props (props, _) ->
          List.iter
            (fun (_, strin) ->
              used := (strin, ct.ctyp_loc.Location.loc_start) :: !used;
            )
            props
      | _ -> ()
    );


  DeadLexiFi.tstr_type :=
    (fun typ ctype ->
      let path =
        String.concat "." @@ List.rev @@
        typ.typ_name.Asttypes.txt :: !mods
        @ (String.capitalize_ascii (unit !current_src):: [])
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
    );


  DeadLexiFi.ttype_of :=
    (fun e ->
      let name = String.concat "." @@ List.rev @@
        !mods @ (String.capitalize_ascii (unit !current_src):: [])
      in
      let call_site =
        if e.exp_loc.Location.loc_ghost then !last_loc
        else e.exp_loc.Location.loc_start
      in
      dyn_rec := (name, e.exp_type, call_site) :: !dyn_rec
    );


  DeadLexiFi.prepare_report :=
    (fun decs ->
      List.iter
        (fun (strin, pos) ->
          hashtbl_find_list str strin
          |> List.iter
            (fun loc ->
              if exported DeadFlag.typ loc then
                LocHash.add_set references loc pos
            )
        )
        !used;
      let rec process (p, typ, call_site) =
        match typ.desc with
        | Tarrow (_, t, _, _) | Tlink t -> process (p, t, call_site)
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
            ( if exported DeadFlag.typ loc then LocHash.add_set references loc
              else ignore
            )
            (hashtbl_find_list dyn_used (get_type path (String.length path - 1)))
        )
        decs
    );
