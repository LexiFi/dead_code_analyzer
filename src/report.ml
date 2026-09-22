type ('storage, 'key) main_report_configuration = {
  title: string;
  section: Config.Sections.main_section;
  data: 'storage;
  get_unused: max_uses:int -> (int, 'key list) Hashtbl.t;
  get_reportable: 'key -> (string * Lexing.position * string);
    (** returns the [filepath], [location], and [path] to report *)
  get_uses: 'key -> Lexing.position list;
}

let main_report_configuration (state : State.t) = function
  | `Value ->
      let title = "UNUSED EXPORTED VALUES" in
      let section = state.config.sections.exported_values in
      let data = state.values in
      let get_unused ~max_uses =
        State.Values.get_unused ~max_uses data
      in
      let get_reportable (val_loc, builddir) =
        let val_path = State.Values.get_val_path ~builddir ~val_loc data in
        match val_path with
        | None -> assert false
        | Some val_path ->
            let filepath = Filename.concat builddir val_loc.Lexing.pos_fname in
            (filepath, val_loc, val_path)
      in
      let get_uses (val_loc, _builddir) =
        State.Values.get_uses ~val_loc data
      in
      `Value { title; section; data; get_unused; get_reportable; get_uses }
  | `Type ->
      let title = "UNUSED CONSTRUCTORS/RECORD FIELDS" in
      let section = state.config.sections.types in
      let data = state.ctors_fields in
      let get_unused ~max_uses =
        State.Ctors_fields.get_unused ~max_uses data
      in
      let get_reportable (cf_loc, builddir) =
        let cf_path = State.Ctors_fields.get_cf_path ~builddir ~cf_loc data in
        match cf_path with
        | None -> assert false
        | Some cf_path ->
            let filepath = Filename.concat builddir cf_loc.Lexing.pos_fname in
            (filepath, cf_loc, cf_path)
      in
      let get_uses (cf_loc, _builddir) =
        State.Ctors_fields.get_uses ~cf_loc data
      in
      `Type { title; section; data; get_unused; get_reportable; get_uses }
  | `Method ->
      let title = "UNUSED METHODS" in
      let section = state.config.sections.methods in
      let data = state.methods in
      let get_unused ~max_uses =
        State.Methods.get_unused ~max_uses data
      in
      let get_reportable (obj_loc, meth_name, builddir) =
        let meth_path =
          State.Methods.get_meth_path ~builddir ~obj_loc ~meth_name data
        in
        match meth_path with
        | None -> assert false
        | Some meth_path ->
            let filepath = Filename.concat builddir obj_loc.Lexing.pos_fname in
            (filepath, obj_loc, meth_path)
      in
      let get_uses (obj_loc, meth_name, _builddir) =
        State.Methods.get_uses ~obj_loc ~meth_name data
      in
      `Method { title; section; data; get_unused; get_reportable; get_uses }

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

let print_section_footer =
  let msg = "Nothing else to report in this section" in
  let separator = String.make 80 '-' in
  fun () -> Printf.printf "\n%s\n%s\n\n\n" msg separator

let print_subsection_separator =
  let separator = "--------" in
  fun () -> Printf.printf "%s\n\n\n" separator

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

let print_uses key report_config =
  let uses = report_config.get_uses key in
  List.fast_sort compare uses
  |> List.iter (fun use_loc ->
      (* TODO: store the use_loc's builddir for better output info *)
      string_of_location ~print_col:true use_loc
      |> Printf.printf "%s\n"
  )

let print_unused keys ~nb_uses report_config =
  let reportable_and_keys =
    (* Reportable infos, sorted in lexicographical order.
       The keys are kept in case the uses must be printed as well.
    *)
    List.map
      (fun key ->
        let reportable = report_config.get_reportable key in
        (reportable, key)
      )
      keys
    |> List.fast_sort (fun rk1 rk2 -> compare (fst rk1) (fst rk2))
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
      match reportable_and_keys with
      | ((filepath, _, _), _)::_ -> ref (Filename.dirname filepath)
      | [] -> ref "" (* Dummy value, there won't be anything to compare with *)
    in
    fun filepath ->
      let dir = Filename.dirname filepath in
      if not (String.equal !prev_dir dir) then
        Printf.printf "\n";
      prev_dir := dir
  in
  let print (reportable, key) =
    let (filepath, loc, path) = reportable in
    print_dir_separator filepath;
    let must_report_call_sites =
      nb_uses > 0 && Config.must_report_call_sites report_config.section
    in
    let call_sites_marker =
      if must_report_call_sites then "    Call sites:\n"
      else ""
    in
    let loc = string_of_location ~filepath loc in
    let path = remove_comp_unit path in
    Printf.printf "%s: %s%s\n" loc path call_sites_marker;
    if must_report_call_sites then
      print_uses key report_config
  in
  List.iter print reportable_and_keys

let report report_config =
  let max_uses = Config.get_main_threshold report_config.section in
  let unused = report_config.get_unused ~max_uses in
  print_section_title report_config.title;
  for nb_uses = 0 to max_uses do
    match Hashtbl.find_opt unused nb_uses with
    | None -> ()
    | Some unused_i ->
        if nb_uses > 0 then
          print_subsection_title ~nb_uses report_config.title;
        print_unused unused_i ~nb_uses report_config;
        if nb_uses < max_uses then
          print_subsection_separator ()
  done;
  print_section_footer ()

let report report_config =
  if Config.must_report_section report_config.section then
    report report_config

let report state section =
  match main_report_configuration state section with
  | `Value report_config -> report report_config
  | `Type report_config -> report report_config
  | `Method report_config -> report report_config
