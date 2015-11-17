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


let rec sign ?(func = false) = function
  | Mty_signature sg -> sg
  | Mty_functor (_, t, _) when func -> begin match t with
    | None -> []
    | Some t -> sign t end
  | Mty_functor (_, _, t) -> sign t
  | Mty_ident _ | Mty_alias _ -> []


let item maker = function
  | Sig_value ({name; _}, {val_loc; _}) -> (name, val_loc)::[]
  | Sig_type ({name=t; _}, {type_kind; _}, _) -> begin match type_kind with
    | Type_record (l, _) -> List.map (fun {Types.ld_id={name; _}; ld_loc; _} -> (t ^ "." ^ name, ld_loc)) l
    | Type_variant l -> List.map (fun {Types.cd_id={name; _}; cd_loc; _} -> (t ^ "." ^ name, cd_loc)) l
    | _ -> [] end
  | Sig_module ({name; _}, {md_type; _}, _)
  | Sig_modtype ({name; _}, {mtd_type = Some md_type; _}) ->
    List.map (fun (n, l) -> (name ^ "." ^ n, l)) (maker md_type)
  | _ -> []

let rec make_content typ =
  List.map (item make_content) (sign typ)
  |> List.flatten


let rec make_arg typ =
  List.map (item make_arg) (sign ~func:true typ)
  |> List.flatten


let expr m = match m.mod_desc with
  | Tmod_apply (m1, m2, _) ->
      let l1 = make_arg m1.mod_type |> List.map (fun (x, _) -> x) in
      let l2 = make_content m2.mod_type in
      List.iter
        (fun (x, loc) -> if List.mem x l1 || l1 = [] then hashtbl_add_to_list references loc m.mod_loc)
        l2
  | _ -> ()
