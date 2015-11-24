(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)


open Types
open Typedtree

open DeadCommon



let equal : (string, string list) Hashtbl.t = Hashtbl.create 16


let defined : string list ref = ref []


let content : (string, (string * Location.t) list) Hashtbl.t = Hashtbl.create 16


let full_name name =
  if List.mem name !defined then
    String.capitalize_ascii (unit !current_src) ^ "." ^ name
  else
    name


let rec sign ?(isfunc = false) = function
  | Mty_signature sg -> sg
  | Mty_functor (_, t, _) when isfunc -> begin match t with
    | None -> []
    | Some t -> sign t end
  | Mty_functor (_, _, t) -> sign t
  | _ -> []


let item ?(clas = false) maker = function
  | Sig_value ({name; _}, {val_loc = loc; _}) -> (name, loc)::[]
  | Sig_type ({name=t; _}, {type_kind; _}, _) -> begin match type_kind with
    | Type_record (l, _) -> List.map (fun {Types.ld_id={name; _}; ld_loc; _} -> (t ^ "." ^ name, ld_loc)) l
    | Type_variant l -> List.map (fun {Types.cd_id={name; _}; cd_loc; _} -> (t ^ "." ^ name, cd_loc)) l
    | _ -> [] end
  | Sig_module ({name; _}, {md_type; _}, _)
  | Sig_modtype ({name; _}, {mtd_type = Some md_type; _}) ->
    List.map (fun (n, l) -> (name ^ "." ^ n, l)) (maker md_type)
  | Sig_class ({name; _}, {cty_loc = loc; _}, _)
  | Sig_class_type ({name; _}, {clty_loc = loc; _}, _) when clas -> (name ^ "#", loc) :: []
  | _ -> []

let rec make_content ?clas typ =
  List.map (item ?clas (make_content ?clas)) (sign typ)
  |> List.flatten


let rec make_arg typ =
  List.map (item make_arg) (sign ~isfunc:true typ)
  |> List.flatten


let rec name ?(default = "_none_") e = match e.mod_desc with
  | Tmod_apply (e, _, _)
  | Tmod_functor (_, _, _, e) -> name ~default e
  | Tmod_ident (path, _) -> Path.name path
  | _ -> default


let expr m = match m.mod_desc with
  | Tmod_apply (m1, m2, _) ->
      let l1 = make_arg m1.mod_type |> List.map (fun (x, _) -> x) in
      let l2 = make_content m2.mod_type in
      List.iter
        (fun (x, loc) -> if List.mem x l1 || l1 = [] then hashtbl_add_to_list references loc m.mod_loc)
        l2
  | _ -> ()

let add_equal mb_expr =

  let path = String.capitalize_ascii (unit !current_src) :: List.rev !mods |> String.concat "." in

  let name = name ~default:path mb_expr in
  let name = full_name name in
  if path <> name then
    hashtbl_add_to_list equal path name;
  make_content ~clas:true mb_expr.mod_type
  |> Hashtbl.replace content path
