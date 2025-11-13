type report_info = {
  filepath: string;
  line_nb : int;
  value : string;
}

let line_of_report_info ri =
  Printf.sprintf "%s:%d:%s" ri.filepath ri.line_nb ri.value

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
