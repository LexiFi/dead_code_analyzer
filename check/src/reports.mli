type report_info = {
  filepath : string;
  line_nb : int;
  value : string;
}

val compare : report_info -> report_info -> int

val line_of_report_info : report_info -> string

(* Format of report lines is : '<file_path>:<line_number>:<value>' with :
   - '<file_path>' not containing any ':'. It is assumed that it has been
     relocated already (see `Path.relocate`).
   - '<line_number>' an int.
   - '<value>' possibly containing ':'.
   Returns a `Result.Ok report_info` upon success.
   Returns a `Result.Error(err, ctx)` usable with `Pretty_print.error` if
   the input does not match the report line format. *)
val report_info_of_line : string -> (report_info, string * string) result

(* If the provided line has the report line format, then its filepath compenent
   is transformed using `f`.
   On Windows, absolute paths start with '<drive>:'. Because, `:` is also the
   separator in report lines, setting `is_windows_path` to true is necessary
   to avoid missplitting a line (`drive:filepath:line_nb:value`
   instead of `filepath:line_nb:value`) *)
val transform_filepath_in_line :
  f:(string -> string) -> is_windows_path:bool -> string -> string
