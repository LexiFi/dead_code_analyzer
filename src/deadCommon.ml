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
let references : (Location.t, Location.t list) Hashtbl.t  = Hashtbl.create 256      (* all value references *)
let corres : (Location.t, Location.t list) Hashtbl.t = Hashtbl.create 256           (* link from dec to def *)


let style : (string * Location.t * string) list ref = ref []                        (* patterns of type unit which are not () *)
let last_loc = ref Location.none                                                    (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""
let mods : string list ref = ref []                                                 (* module path *)



                (********   HELPERS   ********)

let find_path fn = List.find
  (fun path ->
    let lp = String.length path and lf = String.length fn in
      lp >= lf && String.sub path (lp - lf) lf = fn)
  !abspath


let unit fn = try Filename.chop_extension (Filename.basename fn) with _ -> fn


let check_underscore name = not !DeadFlag.underscore || name.[0] <> '_'


let hashtbl_find_list hashtbl key = try Hashtbl.find hashtbl key with Not_found -> []


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

  let export ?(sep = ".") path u id loc =
    if not loc.Location.loc_ghost && u = unit loc.Location.loc_start.Lexing.pos_fname
    && check_underscore id.Ident.name then
      decs := (!current_src,
          String.concat "." (List.rev_map Ident.name path)
          ^ sep
          ^ id.Ident.name,
          loc)
        :: !decs
