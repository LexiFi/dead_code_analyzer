module Path = struct
  type t = string

  (* Convert windows and unix style separator (resp. '\\' and '/') to
     the system's separator, remove any intermediate reference to
     the current directory ("."), and reduce multiple consecutive separators
     into 1.
     WARNING: This assumes `path` is a relative path and will ad "./" at
     the beginning of it after the above manipulation *)
  let normalize path =
    String.split_on_char '\\' path
    |> List.concat_map (String.split_on_char '/')
    |> List.filter (fun s -> s <> "" && s <> ".")
    |> List.cons "."
    |> String.concat Filename.dir_sep

  (* Paths read in res.out points to files in <project_root>/examples/
     relatively from <project_root>/check : '../examples/<rest/of/path>'
     We want to relocate them as relative to the <expected_reports_root>
     directory whjich contains its own examples subdirectory with report files
     organized similarly to <project_root>/examples. :
     './examples/<rest/of/path>'. Therefore, removing the first '.' does the
     trick *)
  let relocate path =
    String.sub path 1 (String.length path - 1)

  let fold ~init ~on_file ~on_directory path =
    if not (Sys.file_exists path) then init
    else if Sys.is_directory path then on_directory init path
    else on_file init path

end


module Section : sig
  (* The results are organized by section in the dead_code_analyzer's output. *)
  type t =
    | Constr_and_fields
    | Methods
    | Opt_always
    | Opt_never
    | Style
    | Threshold of int * t
    | Values

  val to_string : t -> string

  val compare : t -> t -> int

  (* The test suite files have extensions corresponding to the sections of
     the expected results they contain. *)
  val to_extension : t -> string

  (* The test suite files have extensions corresponding to the sections of
     the expected results they contain. *)
  val of_extension : string -> t option

  (* The sections are preceded by headers to help identify them.
     Returns the corresponding section if the given string is header.
     Returns None otherwise. *)
  val of_header : string -> t option

  (* Folowing the header's title is a separator to indicate the start of the
     report for that section *)
  val is_start : string -> bool

  (* There is a footer at the end of each section to indicate the reporting for
     that section is done *)
  val is_end : string -> bool

end = struct

  type t =
    | Constr_and_fields
    | Methods
    | Opt_always
    | Opt_never
    | Style
    | Threshold of int * t
    | Values

  let rec to_string = function
    | Constr_and_fields -> "Constr_and_fields"
    | Methods -> "Methods"
    | Opt_always -> "Opt_always"
    | Opt_never -> "Opt_never"
    | Style -> "Style"
    | Values -> "Values"
    | Threshold (n, t) ->
      let sub_string = to_string t in
      Printf.sprintf "Threshold(%d, %s)" n sub_string

  let compare = compare

  let rec to_extension = function
    | Constr_and_fields -> ".mlit"
    | Methods -> ".mlio"
    | Opt_always -> ".mlopta"
    | Opt_never -> ".mloptn"
    | Style -> ".mlstyle"
    | Values -> ".mli"
    | Threshold (n, base) -> to_extension base ^ string_of_int n

  let rec of_extension = function
    | ".mlit" -> Some Constr_and_fields
    | ".mlio" -> Some Methods
    | ".mlopta" -> Some Opt_always
    | ".mloptn" -> Some Opt_never
    | ".mlstyle" -> Some Style
    | ".mli" -> Some Values
    | ext ->
      let try_threshold prefix =
        if String.starts_with ~prefix ext then
          let fmt = Scanf.format_from_string (prefix ^ "%d") "%d" in
          try
            let n = Scanf.sscanf ext fmt Fun.id in
            of_extension prefix
            |> Option.map (fun constr -> Threshold (n, constr))
          with Scanf.Scan_failure _ -> None
        else None
      in
      let exts = [".mlit"; ".mlio"; ".mlopta"; ".mloptn"; ".mlstyle"; ".mli"] in
      List.find_map try_threshold exts


  let is_start s =
    String.for_all (( = ) '=') s (* = is used for main sections *)
    || String.for_all (( = ) '~') s (* ~ is used for subsections *)

  let is_end s =
    s = "Nothing else to report in this section" (* main sections ending *)
    || String.for_all (( = ) '-') s (* subsections ending *)

  let of_header = function
    | ".> UNUSED CONSTRUCTORS/RECORD FIELDS:" -> Some Constr_and_fields
    | ".> UNUSED METHODS:" -> Some Methods
    | ".> OPTIONAL ARGUMENTS: ALWAYS:" -> Some Opt_always
    | ".> OPTIONAL ARGUMENTS: NEVER:" -> Some Opt_never
    | ".> CODING STYLE:" -> Some Style
    | ".> UNUSED EXPORTED VALUES:" -> Some Values
    | header ->
      let get_threshold prefix constr =
        if String.starts_with ~prefix header then
          let fmt = Scanf.format_from_string (prefix ^ " %d time(s)") "%d" in
          let n = Scanf.sscanf header fmt Fun.id in
          Some (Threshold (n, constr))
        else None
      in
      let get_threshold_constr_and_fields () =
        let prefix = ".>->  ALMOST UNUSED CONSTRUCTORS/RECORD FIELDS: Called" in
        get_threshold prefix Constr_and_fields
      in
      let get_threshold_methods () =
        let prefix = ".>->  ALMOST UNUSED METHODS: Called" in
        get_threshold prefix Methods
      in
      let get_threshold_opt_always () =
        let prefix = ".>->  OPTIONAL ARGUMENTS: ALMOST ALWAYS: Except" in
        get_threshold prefix Opt_always
      in
      let get_threshold_opt_never () =
        let prefix = ".>->  OPTIONAL ARGUMENTS: ALMOST NEVER: Except" in
        get_threshold prefix Opt_never
      in
      let get_threshold_values () =
        let prefix = ".>->  ALMOST UNUSED EXPORTED VALUES: Called" in
        get_threshold prefix Values
      in
      let getters = [
        get_threshold_constr_and_fields;
        get_threshold_methods;
        get_threshold_opt_always;
        get_threshold_opt_never;
        get_threshold_values
      ]
      in
      List.find_map (fun f -> f ()) getters

end


module PP = struct

  let red = "\x1b[31m"
  let green = "\x1b[32m"
  let yellow = "\x1b[33m"
  let blue = "\x1b[34m"
  let white = "\x1b[37m"
  let bg_red = "\x1b[41m"
  let style_reset = "\x1b[0m"

  let error ~err ~ctx () =
    Printf.eprintf "%s%s: %s%s%s%s\n%!" red ctx white bg_red err style_reset

end

module StringSet = Set.Make(String)

module SectionMap = Map.Make(Section)

module State = struct

  type results = {
    success : int;
    fp : int;
    fn : int
  }

  let results_to_string results =
    Printf.sprintf
      "{success = %d; fp = %d; fn = %d}"
      results.success results.fp results.fn

  let empty_results = {success = 0; fp = 0; fn = 0}

  type expected_reports = {
    current_filepath : string option; (* file containg current expected reports *)
    remaining_content : string list; (* expected reports in filename not
                                        observed yet *)
    root : string; (* directory containing the expected reports files*)
    files_map : StringSet.t SectionMap.t (* remaining files containing expected
                                            reports. Once a file is consumed it
                                            is removed from the map. Same for
                                            sections *)
  }

  let expected_reports_to_string expected_reports =
    let current_filepath =
      Option.value ~default:"None" expected_reports.current_filepath
    in
    let remaining_content =
      if List.is_empty expected_reports.remaining_content
      then "[]"
      else "[..]"
    in
    let files_map =
      Printf.sprintf "{ %s\n  }" (
        SectionMap.bindings expected_reports.files_map
        |> List.map (fun (sec, files) ->
            Printf.sprintf "%s ->{%s}"
              (Section.to_string sec)
              (String.concat "; " @@ StringSet.to_list files)
          )
        |> String.concat ";\n    "
      )
        (*
      if SectionMap.is_empty expected_reports.files_map
      then "{}"
      else "{..}"
           *)
    in
    Printf.sprintf
      "{ current_filepath = %s;\n remaining_content = %s;\n root = %s;\n files_map =\n  %s\n}"
      current_filepath remaining_content expected_reports.root files_map

  let empty_expected_reports ={
    current_filepath = None;
    remaining_content = [];
    root = ".";
    files_map = SectionMap.empty
  }

  type t = {
    line : string; (* line observed in dca's report *)
    filepath : string option;
    section : Section.t option; (* current section *)
    expected_reports : expected_reports;
    results : results
  }

  let empty = {
    line = "";
    filepath = None;
    section = None;
    expected_reports = empty_expected_reports;
    results = empty_results
  }

  (* Find all files in root that correspond to test files containing
     expected reports. This files are identified using their extension.
     See module Section above for more info. *)
  let init_expected_reports root =
    let rec on_directory files_map path =
      Sys.readdir path
      |> Array.map (fun filename -> path ^ Filename.dir_sep ^ filename)
      |> Array.fold_left (fun init path -> Path.fold ~init ~on_directory ~on_file path) files_map
    and on_file files_map path =
      let ext = Filename.extension path in
      match Section.of_extension ext with
      | None -> files_map
      | Some sec ->
        let add_to_set = function
          | None -> Some (StringSet.singleton path)
          | Some set -> Some (StringSet.add path set)
        in
        SectionMap.update sec add_to_set files_map
    in
    let init = SectionMap.empty in
    let files_map = Path.fold ~init ~on_directory ~on_file root in
    {empty_expected_reports with files_map; root}

  let init exp_root =
    let expected_reports = init_expected_reports exp_root in
    {empty with expected_reports}

  let incr_fn state =
    let fn = state.results.fn + 1 in
    let results = {state.results with fn} in
    {state with results}

  let report_fn exp_line state =
    PP.error ~err:"Not detected" ~ctx:exp_line ();
    incr_fn state

  let incr_fp state =
    let fp = state.results.fp + 1 in
    let results = {state.results with fp} in
    {state with results}

  let report_fp res_line state =
    PP.error ~err:"Should not be detected" ~ctx:res_line ();
    incr_fp state

  let incr_success state =
    let success = state.results.success + 1 in
    let results = {state.results with success} in
    {state with results}

  let report_success res_line state =
    print_endline res_line;
    incr_success state

  let update_remaining_content state remaining_content =
    let remaining_content = List.filter (( <> ) "") remaining_content in
    let expected_reports = {state.expected_reports with remaining_content} in
    {state with expected_reports}

  let empty_current_file state =
    let clear_current_exp state =
      let er = state.expected_reports in
      let files_map =
        (* Remove file from the expected_reports *)
        let ( let* ) x f = Option.bind x f in
        let ( let+ ) x f = Option.map f x in
        let* sec = state.section in
        let* set = SectionMap.find_opt sec er.files_map in
        let+ filepath = er.current_filepath in
        let set = StringSet.remove filepath set in
        SectionMap.add sec set er.files_map
      in
      let files_map = Option.value files_map ~default:er.files_map in
      let expected_reports =
        {empty_expected_reports with files_map; root = er.root}
      in
      {state with expected_reports}
    in
    let remaining_content = state.expected_reports.remaining_content in
    List.fold_left (Fun.flip report_fn) state remaining_content
    |> clear_current_exp

  let change_file ?(internal = false) filepath state =
    let setup_expected_reports filepath state =
      match state.section with
      | None ->
        let err = "Trying to open a file outside a section" in
        PP.error ~err ~ctx:filepath ();
        state
      | Some sec ->
        let ext = Section.to_extension sec in
        let no_ext =
          try Filename.chop_extension filepath
          with Invalid_argument _ ->
            let err = "Input file without extension" in
            PP.error ~err ~ctx:filepath ();
            filepath
        in
        let exp_filepath = no_ext ^ ext in
        let exp_filepath =
          if internal then exp_filepath
          else
            state.expected_reports.root ^ Filename.dir_sep ^ exp_filepath
            |> Path.normalize
        in
        match SectionMap.find_opt sec state.expected_reports.files_map with
        | Some set when StringSet.mem exp_filepath set ->
          let current_filepath = Some exp_filepath in
          let state =
            In_channel.with_open_text exp_filepath In_channel.input_lines
            |> update_remaining_content state
          in
          let expected_reports =
            {state.expected_reports with current_filepath}
          in
          let filepath = Some filepath in
          {state with expected_reports; filepath}
        | _ ->
          let err = "Expected report not found" in
          PP.error ~err ~ctx:exp_filepath ();
          state (* TODO: report empty section?*)
    in
    empty_current_file state
    |> setup_expected_reports filepath

  let maybe_change_file new_filepath state =
    let compare_no_ext path1 path2 =
      String.compare
        (Filename.remove_extension path1)
        (Filename.remove_extension path2)
    in
    match state.filepath with
    | Some filepath when compare_no_ext filepath new_filepath = 0 ->
      state
    | _ -> change_file new_filepath state

  let empty_current_section state =
    match state.section with
    | None -> state
    | Some sec ->
      let clear_current_section state =
        let er = state.expected_reports in
        let expected_reports =
          let files_map = SectionMap.remove sec er.files_map in
          {er with files_map}
        in
        let section = None in
        {state with section; expected_reports}
      in
      let state = empty_current_file state in
      let remaining_files =
        SectionMap.find_opt sec state.expected_reports.files_map
        |> Option.value ~default:StringSet.empty
      in
      StringSet.fold (change_file ~internal:true) remaining_files state
      |> empty_current_file
      |> clear_current_section

  let change_section section state =
    let state =
      match state.section with
      | None -> state
      | Some sec ->
        let err = "Missing end of section delimiter" in
        let ctx = Section.to_string sec in
        PP.error ~err ~ctx ();
        empty_current_section state
    in
    {state with section}


  let print_results {results; _} =
    let total = results.success + results.fp + results.fn in
    let errors = results.fp + results.fn in
    Printf.printf "Total: %s%d%s\n" PP.blue total PP.style_reset;
    Printf.printf "Success: %s%d%s\n" PP.green results.success PP.style_reset;
    Printf.printf "Failed: %s%d%s\n" PP.red errors PP.style_reset;
    let ratio = 100. *. float_of_int results.success /. float_of_int total in
    let color =
      if ratio < 50. then PP.red
      else if ratio < 80. then PP.yellow
      else PP.green
    in
    Printf.printf "Ratio: %s%F%%%s\n%!" color ratio PP.style_reset

end

(* Format of report lines is : "file_path:line_number: report_info"
   with report_info possibly containing ':'. In case the line comes from
   the direct report of dca (is_res_line), the filepath will be relocated
   to correspond to filepaths coming from expected reports *)
let infos_of_report_line ~is_res_line line =
  let report_line_format = "filepath:line_nb:report_info" in
  match String.split_on_char ':' line with
  | [] | _::[] | _::_::[] ->
    let err =
      Printf.sprintf
        "Unrecognized report line format. Expected : '%s'"
        report_line_format
    in
    PP.error ~err ~ctx:line ();
    None
  | filepath::line_number::report_info ->
    try
      let line_nb = int_of_string line_number in
      let filepath = (* relocate to match expected paths *)
        if is_res_line then Path.relocate filepath
        else filepath
      in
      let filepath = Path.normalize filepath in
      let report_info = String.concat ":" report_info in
      let line = (* recontruct the line with updated fields *)
        if is_res_line then
          String.concat ":" [filepath; line_number; report_info]
        else line
      in
      Some (filepath, line_nb, report_info, line)
    with Failure _int_of_string ->
      let err =
        Printf.sprintf
          "Is not an int. Expected report line format is : '%s'"
          report_line_format
      in
      PP.error ~err ~ctx:line_number ();
      None

let rec process_report_line state (filepath, line_number, report_info, res_line) =
  let state = State.maybe_change_file filepath state in
  match state.expected_reports.remaining_content with
  | [] -> State.report_fp res_line state
  | exp_line::remaining_content when exp_line = res_line ->
    State.update_remaining_content state remaining_content
    |> State.report_success res_line
  | exp_line::remaining_content ->
    match infos_of_report_line ~is_res_line:false exp_line with
    | None ->
      (* exp_line reported in infos_of_report_line as misformatted *)
      state
    | Some (exp_filepath, exp_line_number, _, exp_line) ->
      let compare =
        let paths_compare = String.compare exp_filepath filepath in
        if paths_compare = 0 then exp_line_number - line_number
        else paths_compare
      in
      if compare > 0 then State.report_fp res_line state
      else if compare < 0 then
        let state =
          State.update_remaining_content state remaining_content
          |> State.report_fn exp_line
        in
        process_report_line state (filepath, line_number, report_info, res_line)
      else
        (* The location is fine but report_info does not match.
           The reports are not organized according to the report_info but
           only the locations (including the column which is not reported.
           Check if the current line exists in the remaining_content.
           If so, then it is a successful report which can be removed from
           the remaining content. Otherwise, it is a fp. *)
      if List.mem res_line remaining_content then
        List.filter (( <> ) res_line) remaining_content
        |> State.update_remaining_content state
        |> State.report_success res_line
      else State.report_fp res_line state

let process state res_line =
  let is_report_line, state =
    if res_line = "" then
      false, State.empty_current_file {state with filepath = None}
    else if Section.is_end res_line then
      false, State.empty_current_section state
    else if Section.is_start res_line then
      false, state
    else
      match Section.of_header res_line with
      | Some _ as sec ->
        false, State.change_section sec state
      | None -> (* res_line is a report line *)
        match infos_of_report_line ~is_res_line:true res_line with
        | None ->
          (* res_line reported in infos_of_report_line as misformatted *)
          false, state
        | Some infos ->
          true, process_report_line state infos
  in
  if not is_report_line then print_endline res_line;
  state

let get_expected_reports_root () =
  if (Array.length Sys.argv) < 2 then "."
  else Path.normalize Sys.argv.(1)

let get_res_filename () =
  if (Array.length Sys.argv) < 3 then "res.out"
  else Path.normalize Sys.argv.(2)

let () =
  let res_file = get_res_filename () in
  let input_lines = In_channel.with_open_text res_file In_channel.input_lines in
  let init_state = State.init (get_expected_reports_root ()) in
  let state =
    List.fold_left
      process
      init_state
      input_lines
  in
  State.print_results state
