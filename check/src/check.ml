module PP = Pretty_print

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
            if is_res_file then Path.relocate line |> Path.normalize_to_unix
            else Path.normalize_to_unix line
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
