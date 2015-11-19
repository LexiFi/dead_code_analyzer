(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

                (********   ATTRIBUTES   ********)

let abspath = ref []                                                                (* longest paths known *)


let decs : (string * string * Location.t) list ref = ref []                         (* all exported value declarations *)

let incl : (string * string * Location.t) list ref = ref []                         (* all exported value declarations *)

let references : (Location.t, Location.t list) Hashtbl.t  = Hashtbl.create 256      (* all value references *)
let corres : (Location.t, Location.t list) Hashtbl.t = Hashtbl.create 256           (* link from dec to def *)

let fields : (string, Location.t) Hashtbl.t = Hashtbl.create 256      (* link from fields (record/variant) paths and locations *)

let style : (string * Location.t * string) list ref = ref []                        (* patterns of type unit which are not () *)
let last_loc = ref Location.none                                                    (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""
let mods : string list ref = ref []                                                 (* module path *)



                (********   HELPERS   ********)

let find_path fn ?(sep = '/') l = List.find
  (fun path ->
    let lp = String.length path and lf = String.length fn in
    (lp > lf && path.[lp - lf - 1] = sep || lp = lf) && String.sub path (lp - lf) lf = fn)
  l


let unit fn = try Filename.chop_extension (Filename.basename fn) with _ -> fn


let rec string_cut c s pos len =
  if len = String.length s then s
  else if s.[pos] = c then String.sub s (pos - len) len
  else string_cut c s (pos + 1) (len + 1)
let string_cut c s = string_cut c s 0 0


let check_underscore name = not !DeadFlag.underscore || name.[0] <> '_'


let hashtbl_find_list hashtbl key = try Hashtbl.find hashtbl key with Not_found -> []


let hashtbl_add_to_list hashtbl key elt = Hashtbl.replace hashtbl key (elt :: hashtbl_find_list hashtbl key)


let hashtbl_merge_list tbl1 key1 tbl2 key2 =
  if Hashtbl.mem tbl2 key2 then
    Hashtbl.replace
      tbl1
      key1
      (List.sort_uniq compare (hashtbl_find_list tbl1 key1 @ hashtbl_find_list tbl2 key2))


let exported fn =
  !DeadFlag.exported.print && (!DeadFlag.internal
      || fn.[String.length fn - 1] = 'i'
      || (let src, name = unit !current_src, unit fn in
        String.length name < String.length src
        || String.capitalize_ascii (String.sub name 0 (String.length src)) <> String.capitalize_ascii src)
      || try not (Sys.file_exists (find_path fn !abspath ^ "i")) with Not_found -> true)


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
  | _ -> match find_path loc.loc_start.pos_fname !abspath with
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


let opt_args : (Location.t * string * bool * Location.t) list ref = ref []



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



                (********   PROCESSING  ********)

  let export ?(sep = ".") path u stock id loc =
    if not loc.Location.loc_ghost
    && (u = unit loc.Location.loc_start.Lexing.pos_fname || u <> unit !current_src)
    && check_underscore id.Ident.name then
      stock := (!current_src,
          String.concat "." (List.rev_map Ident.name path)
          ^ sep
          ^ id.Ident.name,
          loc)
        :: !stock



                (**** REPORTING ****)

(* Absolute path *)
let abs loc = match find_path loc.Location.loc_start.pos_fname !abspath with
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


let percent base = 1. -. (float_of_int base) *. (1. -. !DeadFlag.opt.threshold.percentage) /. 10.


(* Base pattern for reports *)
let report s ?(extra = "Called") l continue nb_call pretty_print reporter =
  if nb_call = 0 || l <> [] then begin
    section ~sub:(nb_call <> 0)
    @@ (if nb_call = 0 then s
        else if !DeadFlag.opt.threshold.optional = `Both || extra = "Called" then
          Printf.sprintf "%s: %s %d time(s)" s extra nb_call
        else Printf.sprintf "%s: at least %3.2f%% of the time" s (100. *. percent nb_call));
    List.iter pretty_print l;
    if continue nb_call then
      (if l <> [] then print_endline "--------" else ()) |> print_newline |> print_newline
  end;
  if continue nb_call then reporter (nb_call + 1)
  else (print_newline () |> separator)
