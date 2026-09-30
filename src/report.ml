let get_title ~elt_kind =
  match elt_kind with
  | `Ctor_field -> "UNUSED CONSTRUCTORS/RECORD FIELDS"
  | `Object -> "UNUSED METHODS"
  | `Value -> "UNUSED EXPORTED VALUES"

let print_section_title title =
  (* `.> TITLE:'
     `========='
  *)
  let underline =
    let underline_len = String.length title + 3 in
    String.make underline_len '='
  in
  Printf.printf ".> %s:\n%s\n" title underline

let print_subsection_title ~nb_uses title =
  (* `.>->  ALMOST TITLE: Called n time(s):'
     `~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
  *)
  let title =
    Printf.sprintf "ALMOST %s: Called %d time(s)" title nb_uses
  in
  let underline =
    let underline_len = String.length title + 6 in
    String.make underline_len '~'
  in
  Printf.printf ".>->  %s:\n%s\n" title underline

let print_section_footer : unit -> unit =
  let msg = "Nothing else to report in this section" in
  let separator = String.make 80 '-' in
  fun () -> Printf.printf "\n%s\n%s\n\n\n" msg separator

let print_subsection_separator : unit -> unit =
  let separator = "--------" in
  fun () -> Printf.printf "%s\n\n\n" separator


(** returns the [filepath], [location], and [path] to report *)
let get_reportable ~elt (state : State.t) =
  let (elt_kind, elt_loc, builddir) = elt in
  let elt_path =
    State.get_exported_declaration_path ~elt_kind ~builddir ~elt_loc state
  in
  match elt_path with
  | None -> assert false
  | Some elt_path ->
      let filepath = Filename.concat builddir elt_loc.Lexing.pos_fname in
      (filepath, elt_loc, elt_path)


let get_section_config ~elt_kind (state : State.t) =
  match elt_kind with
  | `Value -> state.config.sections.exported_values
  | `Ctor_field -> state.config.sections.types
  | `Object -> state.config.sections.methods


(** [string_of_location ?filepath ?print_col loc] returns a string of the
    format : "filepath:line" if [not print_col] or "filepath:line:col"
    otherwise, with [filepath] either the one provided or the filename found
    in [loc], and [line] and [col] the position of the location in the file.
    By default [print_col = false]
*)
let string_of_location ?filepath ?(print_col=false) loc =
  let open Lexing in
  let filepath =
    match filepath with
    | None -> loc.pos_fname
    | Some filepath -> filepath
  in
  let line = loc.pos_lnum in
  (* [pos_cnum] is the number of characters since the beginning of the file
     until the current location.
     [pos_bol] is the number of characters since the beginning of the file
     until the beginning of the line *)
  let col = loc.pos_cnum - loc.pos_bol in
  if print_col then
    Printf.sprintf "%s:%d:%d" filepath line col
  else
    Printf.sprintf "%s:%d" filepath line

let print_uses ~elt state =
  let (elt_kind, elt_loc, _builddir) = elt in
  let uses = State.get_uses ~elt_kind ~elt_loc state in
  List.fast_sort compare uses
  |> List.iter (fun use_loc ->
      (* TODO: store the use_loc's builddir for better output info *)
      string_of_location ~print_col:true use_loc
      |> Printf.printf "%s\n"
  )

let print_unused ~elts ~nb_uses state section_config =
  let reportable_and_elts =
    (* Reportable infos, sorted in lexicographical order.
       The elts are kept in case the uses must be printed as well.
    *)
    List.map
      (fun elt ->
        let reportable = get_reportable ~elt state in
        (reportable, elt)
      )
      elts
    |> List.fast_sort (fun re1 re2 -> compare (fst re1) (fst re2))
  in
  let remove_comp_unit path =
    (* Remove the compilation unit from the path. E.g. CompU.Foo.x -> Foo.x
    *)
    let dot_idx = String.index path '.' in
    let suff_len = String.length path - dot_idx - 1 in
    String.sub path (dot_idx + 1) suff_len
  in
  let print_dir_separator =
    (* When there is a change of directory, we print an empty line to ease
       the reading of the results.
       Used during the traversal of the results list.
    *)
    let prev_dir =
      (* Stores the latest dir met. By default, the latest dir is the 1st
         that we will encounter so there will be no extra white line before
         the 1st report
      *)
      match reportable_and_elts with
      | ((filepath, _, _), _)::_ -> ref (Filename.dirname filepath)
      | [] -> ref "" (* Dummy value, there won't be anything to compare with *)
    in
    fun filepath ->
      let dir = Filename.dirname filepath in
      if not (String.equal !prev_dir dir) then
        Printf.printf "\n";
      prev_dir := dir
  in
  let print (reportable, elt) =
    let (filepath, loc, path) = reportable in
    print_dir_separator filepath;
    let must_report_call_sites =
      nb_uses > 0 && Config.must_report_call_sites section_config
    in
    let call_sites_marker =
      if must_report_call_sites then "    Call sites:\n"
      else ""
    in
    let loc = string_of_location ~filepath loc in
    let path = remove_comp_unit path in
    Printf.printf "%s: %s%s\n" loc path call_sites_marker;
    if must_report_call_sites then
      print_uses ~elt state
  in
  List.iter print reportable_and_elts

let report ~elt_kind state section_config =
  let title = get_title ~elt_kind in
  let max_uses = Config.get_main_threshold section_config in
  let unused = State.get_unused ~elt_kind ~max_uses state in
  print_section_title title;
  for nb_uses = 0 to max_uses do
    match Hashtbl.find_opt unused nb_uses with
    | None -> ()
    | Some elts ->
        if nb_uses > 0 then
          print_subsection_title ~nb_uses title;
        print_unused ~elts ~nb_uses state section_config;
        if nb_uses < max_uses then
          print_subsection_separator ()
  done;
  print_section_footer ()

let report ~elt_kind state =
  let section_config = get_section_config ~elt_kind state in
  if Config.must_report_section section_config then
    report ~elt_kind state section_config
