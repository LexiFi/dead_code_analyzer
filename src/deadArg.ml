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


let later = ref []
let last = ref []

let met = Hashtbl.create 512


let eom () =
  List.iter (fun f -> f ()) !later;
  later := [];
  Hashtbl.reset met


let clean loc lab =
  assert (unit loc.Location.loc_start.pos_fname = unit !current_src);
  try
    let filter used = fun (pos, name, has_val, _) ->
      has_val = used && pos = loc && name = lab in
    let l1 = List.filter (filter true) !opt_args in
    let l2 = List.filter (filter false) !opt_args in
    let l1 = List.length l1 in
    let l2 = List.length l2 in
    let ratio1 = float_of_int l1 /. float_of_int (l1 + l2) in
    let ratio2 = float_of_int l2 /. float_of_int (l1 + l2) in
    let good (opt: DeadFlag.opt) ratio len =
      (opt.threshold.optional <> `Both || len <= opt.threshold.exceptions)
      && ratio >= opt.threshold.percentage
    in

    if not (good !DeadFlag.opta ratio1 l2 || good !DeadFlag.opta ratio2 l1
        || good !DeadFlag.optn ratio1 l2 || good !DeadFlag.optn ratio2 l1) then
      opt_args := List.filter (fun (pos, name, _, _) -> loc <> pos && lab <> name) !opt_args
  with _ -> ()


let add lab expr loc last_loc nb_occur =
  let has_val = match expr.exp_desc with
    | Texp_construct (_, {cstr_name = "None"; _}, _) -> false
    | _ -> true
  in
  let occur =
    let occur =
      if not (Hashtbl.mem nb_occur lab) then Hashtbl.add nb_occur lab 1
      else Hashtbl.find nb_occur lab + 1 |> Hashtbl.replace nb_occur lab;
      Hashtbl.find nb_occur lab
    in ref occur
  in
  let call_site = if expr.exp_loc.Location.loc_ghost then last_loc else expr.exp_loc in
  if check_underscore lab then
    let loc = VdNode.find loc lab occur in
    if not (Hashtbl.mem met (last_loc, loc, lab)) then begin
      Hashtbl.add met (last_loc, loc, lab) ();
      opt_args := (loc, lab, has_val, call_site) :: !opt_args
    end


let rec process loc args =

  List.iter       (* treat each arg's expression before all (even if ghost) *)
    (function
      | (_, Some e) -> check e
      | _ -> ())
    args;

  if loc.Location.loc_ghost then ()   (* Ghostbuster *)
  else begin                              (* else: `begin ... end' for aesthetics *)
    let nb_occur = Hashtbl.create 256 in
    let last_loc = !last_loc in
      (* last_loc fixed to avoid side effects if added to later/last *)
    let add lab expr = add lab expr loc last_loc nb_occur in
    let add = function
      | (Asttypes.Optional lab, Some expr) ->
          if VdNode.is_end loc
          && (let fn = loc.Location.loc_start.pos_fname in fn.[String.length fn - 1] = 'i') then
            last := (fun () -> add lab expr) :: !last
          else if VdNode.is_end loc && !depth > 0 then
            later := (fun () -> add lab expr) :: !later
          else
            add lab expr
      | _ -> ()
    in
    List.iter add args
  end


(* Verify the nature of the argument to detect and treat function applications and uses *)
and check e =
  (* Optional arguments used to match a signature are considered used *)
  let get_sig_args typ =
    let rec loop args typ =
      match typ.desc with
      | Tarrow (Asttypes.Optional _ as arg, _, t, _) ->
          loop ((arg, Some {e with exp_desc = Texp_constant (Asttypes.Const_int 0)})::args) t
      | Tarrow (_, _, t, _)
      | Tlink t -> loop args t
      | _ -> args
    in loop [] typ
  in

  match e.exp_desc with
  | Texp_ident (_, _, {val_loc; _}) ->
      process val_loc (get_sig_args e.exp_type)
  | Texp_apply ({exp_desc = Texp_ident (_, _, {val_loc; _}); _}, _)
  | Texp_apply ({exp_desc = Texp_field (_, _, {lbl_loc = val_loc; _}); _}, _) ->
      process val_loc (get_sig_args e.exp_type);
      if not val_loc.Location.loc_ghost then
        last_loc := val_loc
  | Texp_let (* Partial application as argument may cut in two parts:
              * let _ = partial in implicit opt_args elimination *)
      ( _,
        [{vb_expr = {exp_desc = Texp_apply ({exp_desc = Texp_ident (_, _, {val_loc; _}); _}, _); _}; _}],
        { exp_desc = Texp_function (_,
            [{c_lhs = {pat_desc = Tpat_var (_, _); pat_loc = {loc_ghost = true; _}; _};
              c_rhs = {exp_desc = Texp_apply (_, args); exp_loc = {loc_ghost = true; _}; _}; _}],_);
          exp_loc = {loc_ghost = true; _};_}) ->
      process val_loc args
  | _ -> ()


let rec node_build loc expr =
  match expr.exp_desc with
  | Texp_function (lab, [{c_lhs = {pat_type; _}; c_rhs = exp; _}], _) ->
      DeadType.check_style pat_type expr.exp_loc;
      begin match lab with
      | Asttypes.Optional s ->
          if !DeadFlag.optn.print || !DeadFlag.opta.print then
            let (opts, next) = VdNode.get loc in
            VdNode.update loc (s :: opts, next);
          node_build loc exp
      | _ -> () end
  | Texp_apply ({exp_desc = Texp_ident (_, _, {val_loc = loc2; _}); _}, args)
  | Texp_apply ({exp_desc = Texp_field (_, _, {lbl_loc = loc2; _}); _}, args)
    when !DeadFlag.optn.print || !DeadFlag.opta.print ->
      process loc2 args;
      VdNode.merge_locs loc loc2
  | Texp_ident (_, _, {val_loc = loc2; _})
    when !DeadFlag.optn.print || !DeadFlag.opta.print ->
      VdNode.merge_locs loc loc2
  | _ -> ()



                (********   WRAPPING  ********)


let wrap f x y =
  if DeadFlag.(!optn.print || !opta.print) then f x y else ()

let process val_loc args =
  wrap process val_loc args
