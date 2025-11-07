module PP = Pretty_print

module Path = struct

  (* Convert windows and unix style separator (resp. '\\' and '/') to
     the system's separator, remove any intermediate reference to
     the current directory ("."), and reduce multiple consecutive separators
     into 1.
     WARNING: If `path` is a relative path, it will ad "./" at the beginning
     after the above manipulation *)
  let normalize path =
    let splitted_path =
      String.split_on_char '\\' path
      |> List.concat_map (String.split_on_char '/')
    in
    match splitted_path with
    | ""::_ -> (* do not relocate aboslute paths *)
      String.concat Filename.dir_sep splitted_path
    | _ ->
      splitted_path
      |> List.filter (fun s -> s <> "" && s <> ".")
      |> List.cons "."
      |> String.concat Filename.dir_sep

  (* Paths read in res.out points to files in '<project_root>/examples/'
     relatively from that directory :
       - for files in the 'using_make' subdirectory :
         '<project_root>/examples/using_make<path/to/file>' with
         '<project_root>' an absolute path.
       - for files in the 'using_dune' subdirectory :
         '/workspace_root/<path/to/file>' on unix;
         '<project_root>/examples/using_dune/_build/default/<path/to/file>' on windows
     We want to relocate them as relative to the <expected_reports_root>
     directory which contains its own examples subdirectory with report files
     organized similarly to '<project_root>/examples/' :
     './examples/<rest/of/path>'.
     - For files in 'using_make': there is no 'using_make' file or directory in
       '<project_root>/examples/using_make'. Therefore, taking everything in the
       path from the last 'using_make' filename provides the common part for the
       relocation. Then, adding an extra './' completes the path as found in the
       expected reports
     - For files in 'using_dune' on unix: replacing '/workspace_root' with
       './examples/using_dune' is sufficient
     - For files in 'using_dune' on windows: the same startegy as for files in
       'using_make' can be applied with the removal of '_build/default' on the
       way. because those directory names are unique, we can actually share the
       same logic for both 'using_make' and 'using_dune' on windows. *)
  let relocate path =
    let dune_build_prefix =
      String.concat Filename.dir_sep [""; "workspace_root"; ""]
    in
    let origin =
      if String.starts_with ~prefix:dune_build_prefix path then
        `Using_dune (Sys.unix)
      else `Using_make
    in
    let relative_path = (* ["using<make|dune>"; <path/to/file>...] *)
      match origin with
      | `Using_dune true ->
        let prefix_len = String.length dune_build_prefix in
        let path_len = String.length path in
        let no_prefix =
          (* remove the leading dune_build_prefix *)
          String.sub path prefix_len (path_len - prefix_len)
        in
        "using_dune"::no_prefix::[]
      | `Using_dune false | `Using_make ->
        (* retrieve the subpath starting from the last occurence of
           "using_<make|dune>" if it exists, and remove "_build" and "default"
           directories from the path *)
        let rec extract_subpath acc dirpath =
          let basename = Filename.basename dirpath in
          if basename = "using_make" || basename = "using_dune" then
            basename::acc
          else if basename = dirpath (* fixpoint *) then
            path::[] (* TODO: error handling *)
          else
            let acc =
              match basename with
              | "_build" | "default" -> acc
              | basename -> basename::acc
            in
            extract_subpath acc (Filename.dirname dirpath)
        in
        extract_subpath [] path
    in
    String.concat Filename.dir_sep ("."::"examples"::relative_path)

end

module Reports = struct

  type report_info = {
    filepath: string;
    line_nb : int;
    value : string;
  }

  let line_of_report_info ri =
    Printf.sprintf "%s:%d:%s" ri.filepath ri.line_nb ri.value

  (* Format of report lines is : "file_path:line_number: value"
     with value possibly containing ':'. It is assumed that the file_path has
     been relocated already *)
  let report_info_of_line line =
    let fmt_error ~ctx ~fmt =
      let report_line_format = "filepath:line_nb:value" in
      let err = Printf.sprintf fmt report_line_format in
      Error (err, ctx)
    in
    let report_info_of_raw_data filepath line_nb value =
      try
        let line_nb = int_of_string line_nb in
        let filepath = Path.normalize filepath in
        let value = String.concat ":" value in
        Ok {filepath; line_nb; value}
      with Failure _int_of_string ->
        fmt_error ~ctx:line_nb
                  ~fmt:"Is not an int. Expected report line format is : '%s'"
    in
    match String.split_on_char ':' line with
    | [] | _::[] | _::_::[] ->
      (* Missing elements : format not matched *)
      fmt_error ~ctx:line
                ~fmt:"Unrecognized report line format. Expected : '%s'"
    | filepath::line_nb::value ->
      report_info_of_raw_data filepath line_nb value

  let transform_filepath_in_line ~f ~is_windows_path line =
    match String.split_on_char ':' line with
    | [] | _::[] | _::_::[] -> line
    | drive::filepath::line_nb::value when is_windows_path->
      (* On Windows, paths start with '<drive>:', and get a split on ':' *)
      let filepath = Printf.sprintf "%s:%s" drive filepath in
      let filepath = f filepath in
      String.concat ":" (filepath :: line_nb :: value)
    | filepath::line_nb::value ->
      let filepath = f filepath in
      String.concat ":" (filepath :: line_nb :: value)

end

module State = struct
  type t = {
    scores : Scores.t
  }

  let empty = {
    scores = Scores.init
  }

  let incr_fn state =
    let scores = Scores.incr_fn state.scores in
    {scores}

  let report_fn ri state =
    let ctx = Reports.line_of_report_info ri in
    PP.error ~err:"Not detected" ~ctx;
    incr_fn state

  let incr_fp state =
    let scores = Scores.incr_fp state.scores in
    {scores}

  let report_fp ri state =
    let ctx = Reports.line_of_report_info ri in
    PP.error ~err:"Should not be detected" ~ctx;
    incr_fp state

  let incr_success state =
    let scores = Scores.incr_success state.scores in
    {scores}

  let report_success ri state =
    let line = Reports.line_of_report_info ri in
    print_endline line;
    incr_success state

end

let maybe_report state report_fun line =
  match Reports.report_info_of_line line with
  | Ok ri -> report_fun ri state
  | Error _ ->
    print_endline line;
    state

let rec process state exp_lines got_lines =
  match exp_lines, got_lines with
  | [], [] -> state
  | [], got::got_lines ->
    let state = maybe_report state State.report_fp got in
    process state exp_lines got_lines
  | exp::exp_lines, [] ->
    let state = maybe_report state State.report_fn exp in
    process state exp_lines got_lines
  | exp::exp_lines, got::got_lines when String.equal exp got ->
    let state = maybe_report state State.report_success exp in
    process state exp_lines got_lines
  | ""::exp_lines, _ -> (* ignore empty line mismatch *)
    process state exp_lines got_lines
  | _, ""::got_lines -> (* ignore empty line mismatch *)
    process state exp_lines got_lines
  | _, _ -> (* exp <> got *)
    process_mismatch state exp_lines got_lines

and process_mismatch state exp_lines got_lines =
  match exp_lines, got_lines with
  | [], _ | _, [] -> process state exp_lines got_lines
  | exp::_, got::_ when String.equal exp got ->
    process state exp_lines got_lines
  | exp::exp_lines', got::got_lines' -> (* mismatch: exp <> got *)
    let consume_exp state =
      let state = maybe_report state State.report_fn exp in
      process state exp_lines' got_lines
    in
    let consume_got state =
      let state = maybe_report state State.report_fp got in
      process state exp_lines got_lines'
    in
    (* In order:
       1. handle if a section header is involved
       2. handle if a section end is involved
       3. handle if a section start is involved
       4. handle report line mismatch
    *)
    let handle_headers alt state =
      match Section.of_header exp, Section.of_header got with
      | Some exp_sec, Some got_sec ->
        (* both file reached a section header. Consume the section that is
           expected to appear first in the analyzer's report *)
        let comp = Section.compare exp_sec got_sec in
        if comp < 0 then consume_exp state
        else (* > 0 *)   consume_got state
      | None, Some _ -> consume_exp state
      | Some _, None -> consume_got state
      | None, None -> alt state
    in
    let handle_section_end alt state =
      (* consume lines in the file that did not reach a main section end
         when the other did *)
      let exp_is_end = Section.is_end exp in
      let got_is_end = Section.is_end got in
      if exp_is_end && (not got_is_end || Section.is_sub_end got) then
        consume_got state
      else if got_is_end && (not exp_is_end || Section.is_sub_end exp) then
        consume_exp state
      else (
        assert (not exp_is_end && not got_is_end);
        alt state
      )
    in
    let handle_section_start alt state =
      if Section.is_start exp && Section.is_start got then (
        let ctx = "Start of section mismatch" in
        let err = Printf.sprintf "\nexpected: \"%s\"\ngot: \"%s\"" exp got in
        PP.error ~err ~ctx;
        process state exp_lines' got_lines'
      )
      else alt state
    in
    let handle_report_infos alt state =
      (* If either is not a valid report line, then it is consumed.
         If both are valid report_lines then the first in lexicographical order
         is consumed *)
      let exp_ri = Reports.report_info_of_line exp in
      let got_ri = Reports.report_info_of_line got in
      match exp_ri, got_ri with
      | Ok _, Ok _ ->
        let comp = String.compare exp got in
        assert (comp <> 0);
        if comp < 0 then consume_exp state
        else (* > 0 *)   consume_got state
      | Ok _, Error (err, ctx) ->
        PP.error ~err ~ctx;
        consume_got state
      | Error (err, ctx), Ok _ ->
        PP.error ~err ~ctx;
        consume_exp state
      | Error _, Error _ -> alt state
    in
    let default state =
      let ctx = "Line mismatch" in
      let err = Printf.sprintf "\nexpected: \"%s\"\ngot: \"%s\"" exp got in
      PP.error ~err ~ctx;
      process state exp_lines' got_lines'
    in
    let handle_mismatch =
      handle_headers
      @@ handle_section_end
      @@ handle_section_start
      @@ handle_report_infos
      @@ default
    in
    handle_mismatch state


let get_expected_reports_filename () =
  if (Array.length Sys.argv) < 2 then
    failwith "Missing expected reports file (ext=.exp)"
  else Path.normalize Sys.argv.(1)

let get_res_filename () =
  if (Array.length Sys.argv) < 3 then
    failwith "Missing result file (ext=.got)"
  else Path.normalize Sys.argv.(2)

let normalized_lines_of ~is_res_file filename =
  let normalize line =
    match Section.of_header line with
    | Some _ -> (* guard against transforming the OPTIONAL ARGUMENTS sections *)
      line
    | None ->
      Reports.transform_filepath_in_line line
        ~is_windows_path:(not Sys.unix && is_res_file)
        ~f:(fun line ->
            if is_res_file then Path.relocate line |> Path.normalize
            else Path.normalize line
        )
  in
  In_channel.with_open_text filename In_channel.input_lines
  |> List.map normalize

let () =
  let exp_lines =
    (get_expected_reports_filename ())
    |> normalized_lines_of ~is_res_file:false
  in
  let got_lines =
    (get_res_filename ())
    |> normalized_lines_of ~is_res_file:true
  in
  let init_state = State.empty in
  let state = process init_state exp_lines got_lines in
  Scores.pp state.scores
