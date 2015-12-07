(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

                (********   ATTRIBUTES   ********)

let abspath : (string, string) Hashtbl.t = Hashtbl.create 256                  (* longest paths known *)


let decs : (Location.t, string * string) Hashtbl.t = Hashtbl.create 256                         (* all exported value declarations *)

let incl : (Location.t, string * string) Hashtbl.t = Hashtbl.create 256                         (* all exported value declarations *)

let references : (Location.t, Location.t) Hashtbl.t  = Hashtbl.create 256      (* all value references *)

let fields : (string, Location.t) Hashtbl.t = Hashtbl.create 256      (* link from fields (record/variant) paths and locations *)

let style : (string * Location.t * string) list ref = ref []                        (* patterns of type unit which are not () *)
let last_loc = ref Location.none                                                    (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""
let mods : string list ref = ref []                                                 (* module path *)


let depth = ref (-1)

let _none = "_none_"
let _obj = "*obj*"
let _include = "*include*"
let _variant = ": variant :"

                (********   HELPERS   ********)

let unit fn = try Filename.chop_extension (Filename.basename fn) with _ -> fn


let rec string_cut c s pos len =
  if len = String.length s then s
  else if s.[pos] = c then String.sub s (pos - len) len
  else string_cut c s (pos + 1) (len + 1)
let string_cut c s = string_cut c s 0 0


let check_underscore name = not !DeadFlag.underscore || name.[0] <> '_'


let hashtbl_find_list hashtbl key = Hashtbl.find_all hashtbl key

let hashtbl_add_to_list hashtbl key elt = Hashtbl.add hashtbl key elt

let hashtbl_add_unique_to_list hashtbl key elt =
  if not (List.mem elt (hashtbl_find_list hashtbl key)) then
    Hashtbl.add hashtbl key elt

let rec hashtbl_remove_list hashtbl key =
  if Hashtbl.mem hashtbl key then begin
    Hashtbl.remove hashtbl key;
    hashtbl_remove_list hashtbl key
  end

let hashtbl_replace_list hashtbl key l =
  hashtbl_remove_list hashtbl key;
  List.iter (fun elt -> hashtbl_add_to_list hashtbl key elt) l

let hashtbl_merge_list tbl1 key1 tbl2 key2 =
  List.iter (fun elt -> hashtbl_add_to_list tbl1 key1 elt) (hashtbl_find_list tbl2 key2)

let hashtbl_merge_unique_list tbl1 key1 tbl2 key2 =
  List.iter (fun elt -> hashtbl_add_unique_to_list tbl1 key1 elt) (hashtbl_find_list tbl2 key2)


let find_path fn ?(sep = '/') l = List.find
  (fun path ->
    let lp = String.length path and lf = String.length fn in
    (lp > lf && path.[lp - lf - 1] = sep || lp = lf) && String.sub path (lp - lf) lf = fn)
  l

let find_abspath fn =
  find_path fn (hashtbl_find_list abspath (unit fn))




let exported flag loc =
  let fn = loc.Location.loc_start.pos_fname in
  !flag.DeadFlag.print
  && hashtbl_find_list references loc |> List.length <= !flag.DeadFlag.threshold
  && (flag == DeadFlag.typ
    || !DeadFlag.internal
    || fn.[String.length fn - 1] = 'i'
    || unit !current_src <> unit fn
    || try not (Sys.file_exists (find_abspath fn ^ "i")) with Not_found -> true)


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
let prloc ?(call_site = false) ?fn (loc : Location.t) =
  begin match fn with
  | Some s ->
      print_string (Filename.dirname s ^ "/" ^ Filename.basename loc.loc_start.pos_fname)
  | _ -> match find_abspath loc.loc_start.pos_fname with
    | s -> print_string s
    | exception Not_found -> Printf.printf "!!UNKNOWN<%s>!!%!" loc.loc_start.pos_fname
  end;
  print_char ':';
  print_int loc.loc_start.pos_lnum;
  if call_site then begin
    print_char ':';
    print_int (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
  end
  else
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


let local_locs = ref []

let keep_loc : (Location.t, unit) Hashtbl.t = Hashtbl.create 32


(* Get or create a vd_node corresponding to the location *)
let vd_node ?(add = false) loc =
  assert (not loc.Location.loc_ghost);
  try (Hashtbl.find vd_nodes loc)
  with Not_found ->
    let rec r = {loc; ptr = r; opt_args = []} in
    if add then
      Hashtbl.add vd_nodes loc r;
    let fn = loc.Location.loc_start.pos_fname in
    if add
    && (!depth > 0 && unit fn = unit !current_src
        || !depth = 0 && fn.[String.length fn - 1] <> 'i'
        && try (Sys.file_exists (find_abspath fn ^ "i")) with Not_found -> false) then
      local_locs := loc :: !local_locs;
    r

(* Locations l1 and l2 are part of a binding from one to another *)
let merge_locs ?add l1 l2 =
  if not l1.Location.loc_ghost && not l2.Location.loc_ghost then begin

    let vd1 = vd_node ?add l1 in
    let vd2 = vd_node l2 in

    let keep loc =
      local_locs := List.filter ((<>) loc) !local_locs
    in
    let met = Hashtbl.create 8 in
    let rec keep_repr node =
      keep node.loc;
      if node.ptr != node && not (Hashtbl.mem met node) then begin
        Hashtbl.add met node ();
        keep_repr node.ptr
      end
    in

    vd1.ptr.ptr <- vd2.ptr;
    keep_repr vd1;
  end



                (********   PROCESSING  ********)

let export ?(sep = ".") path u stock id loc =
  let value =
    String.concat "." (List.rev_map Ident.name path)
    ^ sep
    ^ id.Ident.name
  in
  (* a .cmi file can contain locations from other files.
    For instance:
        module M : Set.S with type elt = int
    will create value definitions whose location is in set.mli
  *)
  if not loc.Location.loc_ghost
  && (u = unit loc.Location.loc_start.Lexing.pos_fname || u == _include)
  && check_underscore id.Ident.name then
    hashtbl_add_to_list stock loc (!current_src, value)



                (**** REPORTING ****)

(* Absolute path *)
let abs loc = match find_abspath loc.Location.loc_start.pos_fname with
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
      prloc ~call_site:true loc |> print_newline
  | _ ->          (* first ghost met *)
      print_endline "~ ghost ~";
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


let report_basic ?folder decs title (flag:DeadFlag.basic) =
  let folder = match folder with
    | Some folder -> folder
    | None -> fun nb_call -> fun loc (fn, path) acc ->
        let rec cut_main s pos =
          if pos = String.length s then s
          else if s.[pos] = '.' then String.sub s (pos + 1) (String.length s - pos - 1)
          else cut_main s (pos + 1)
        in
        let test elt = match hashtbl_find_list references elt with
          | l when check_length nb_call l -> Some ((fn, cut_main path 0, loc, l) :: acc)
          | _ -> None
        in match test loc with
          | exception Not_found when nb_call = 0 ->
                (fn, cut_main path 0, loc, []) :: acc
          | exception Not_found -> acc
          | None -> acc
          | Some l -> l
  in
  let rec reportn nb_call =
    let l =
     Hashtbl.fold (folder nb_call) decs []
      |> List.fast_sort (fun (fn1, path1, loc1, _) (fn2, path2, loc2, _) ->
          compare (fn1, loc1, path1) (fn2, loc2, path2))
    in

    let change =
      let (fn, _, _, _) = try List.hd l with _ -> (_none, "", !last_loc, []) in
      dir fn
    in
    let pretty_print = fun (fn, path, loc, call_sites) ->
      if change fn then print_newline ();
      prloc ~fn loc;
      print_string path;
      if call_sites <> [] && flag.call_sites then print_string "    Call sites:";
      print_newline ();
      if flag.call_sites then begin
        List.fast_sort compare call_sites
        |> List.iter (pretty_print_call ());
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call = nb_call < flag.threshold in
    let s =
      if nb_call = 0 then title
      else "ALMOST " ^ title
    in
    report s l continue nb_call pretty_print reportn

  in reportn 0



                (********   LEXIFI SPECIALS ********)

module DeadLexiFi = struct
(*   .^.  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  .^.   *)
(*  / ! \  DO NOT DELETE UNLESS YOU CAN COMPILE WITH `make lexifi' AND YOU KNOW WHAT YOU ARE DOING  / ! \  *)
(* /_____\   /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\   /_____\ *)

  (* The following hooks are pointing LexiFi's extensions if compiled with `make lexifi'.
   * They stay as following otherwise *)

  let sig_value : (Types.value_description -> unit) ref =
    ref (fun _ -> ())

  let export_type : (Location.t -> string -> unit) ref =
    ref (fun _ _ -> ())

  let type_ext : (Typedtree.core_type -> unit) ref =
    ref (fun _ -> ())

  let tstr_type : (Typedtree.type_declaration -> string -> unit) ref =
    ref (fun _ _ -> ())

  let ttype_of : (Typedtree.expression -> unit) ref =
    ref (fun _ -> ())

  let prepare_report : ((Location.t, string * string) Hashtbl.t -> unit) ref =
    ref (fun _ -> ())
end
