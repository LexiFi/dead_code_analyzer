module PP = Pretty_print

module Path = struct

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
     directory whhich contains its own examples subdirectory with report files
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

module StringSet = Set.Make(String)
module SectionMap = Map.Make(Section)

module Reports = struct
  type t = {
    current_filepath : string option; (* file containg current expected reports *)
    remaining_content : string list; (* expected reports in filename not
                                        observed yet *)
    root : string; (* directory containing the expected reports files*)
    files_map : StringSet.t SectionMap.t (* remaining files containing expected
                                            reports. Once a file is consumed it
                                            is removed from the map. Same for
                                            sections *)
  }

  let empty ={
    current_filepath = None;
    remaining_content = [];
    root = ".";
    files_map = SectionMap.empty
  }

  (* Find all files in root that correspond to test files containing
     expected reports. This files are identified using their extension.
     See module Section above for more info. *)
  let init root =
    let rec on_directory files_map path =
      Sys.readdir path
      |> Array.map (fun filename -> Filename.concat path filename)
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
    {empty with files_map; root}

  (* useful for debug *)
  let[@warning "-32"] to_string ?(show_content=true) expected_reports =
    if not show_content then
      if SectionMap.is_empty expected_reports.files_map then "{}"
      else "{..}"
    else
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
      in
      Printf.sprintf
        "{ current_filepath = %s;\n remaining_content = %s;\n root = %s;\n files_map =\n  %s\n}"
        current_filepath remaining_content expected_reports.root files_map
end

module State = struct
  type t = {
    filepath : string option;
    section : Section.t option; (* current section *)
    expected_reports : Reports.t;
    scores : Scores.t
  }

  let empty = {
    filepath = None;
    section = None;
    expected_reports = Reports.empty;
    scores = Scores.init
  }

  let init exp_root =
    let expected_reports = Reports.init exp_root in
    {empty with expected_reports}

  let incr_fn state =
    let scores = Scores.incr_fn state.scores in
    {state with scores}

  let report_fn exp_line state =
    PP.error ~err:"Not detected" ~ctx:exp_line ();
    incr_fn state

  let incr_fp state =
    let scores = Scores.incr_fp state.scores in
    {state with scores}

  let report_fp res_line state =
    PP.error ~err:"Should not be detected" ~ctx:res_line ();
    incr_fp state

  let incr_success state =
    let scores = Scores.incr_success state.scores in
    {state with scores}

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
        {Reports.empty with files_map; root = er.root}
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
            Filename.concat state.expected_reports.root exp_filepath
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
  let state = List.fold_left process init_state input_lines in
  Scores.pp state.scores
