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
let depth = ref (-1)

let met = Hashtbl.create 512


let eom () =
  List.iter (fun f -> f ()) !later;
  later := [];
  Hashtbl.reset met


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
    let last_loc = !last_loc in

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
        let loc = locate loc in
        if not (Hashtbl.mem met (last_loc, loc, lab)) then begin
          Hashtbl.add met (last_loc, loc, lab) ();
          opt_args := (loc, lab, has_val,
              if expr.exp_loc.Location.loc_ghost then last_loc
              else expr.exp_loc)
            :: !opt_args
        end
    in

    List.iter
      (function
        | (Asttypes.Optional lab, Some expr, _) ->
            if loc.ptr == loc
            && (let fn = loc.loc.Location.loc_start.pos_fname in fn.[String.length fn - 1] = 'i') then
              last := (fun () -> treat lab expr) :: !last
            else if loc.ptr == loc && !depth > 0 then
              later := (fun () -> treat lab expr) :: !later
            else
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
  in
  let get_sig_args typ = get_sig_args [] typ

  in match e.exp_desc with

    | Texp_apply ({exp_desc=Texp_ident (_, _, {val_loc; _}); _}, _)
    | Texp_apply ({exp_desc=Texp_field (_, _, {lbl_loc=val_loc; _}); _}, _) ->
        process val_loc (get_sig_args e.exp_type);
        if not val_loc.Location.loc_ghost then
          last_loc := val_loc

    | Texp_ident (_, _, {val_loc; _}) ->
        process val_loc (get_sig_args e.exp_type)

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



                (********   WRAPPING  ********)


let wrap f x y =
  if DeadFlag.(!opt.never || !opt.always) then f x y else ()

let process val_loc args =
  wrap process val_loc args
