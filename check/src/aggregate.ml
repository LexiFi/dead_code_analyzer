module PP = Pretty_print
module StringSet = Set.Make(String)

let print_title title =
  let underline = String.make (String.length title) '~' in
  Printf.printf "%s%s\n%s%s\n" PP.bold title underline PP.style_reset

let print_title_box title =
  let title_len = String.length title in
  let underline = String.make title_len '~' in
  let filler = String.make (title_len - 2) ' ' in
  (* print_title_box *)
  Printf.printf "%s%s\n|%s|\n%s\n|%s|\n%s%s\n"
    PP.bold underline filler title filler underline PP.style_reset

let print_res title scores =
  print_title_box title;
  Scores.pp scores

module State = struct
  type t = {
    scores : Scores.t;
    unique_success_lines : StringSet.t;
    unique_failure_lines : StringSet.t
  }

  let init = {
    scores = Scores.init;
    unique_success_lines = StringSet.empty;
    unique_failure_lines = StringSet.empty
  }

  let set_scores total failed state =
    let scores =
      Scores.set_failures failed state.scores
      |> Scores.set_success (total - failed)
    in
    {state with scores}

  let join state1 state2 =
    let unique_success_lines =
      StringSet.union state1.unique_success_lines state2.unique_success_lines
    in
    let unique_failure_lines =
      StringSet.union state1.unique_failure_lines state2.unique_failure_lines
    in
    let total = Scores.total state1.scores + Scores.total state2.scores in
    let failed = Scores.failed state1.scores + Scores.failed state2.scores in
    set_scores total failed
      {init with unique_success_lines; unique_failure_lines}

end

let update state line =
  let add_unique_line set =
    let rec make_unique_to_set n =
      let new_line = line ^ string_of_int n in
      if StringSet.mem new_line set then make_unique_to_set (n + 1)
      else new_line
    in
    let line = make_unique_to_set 0 in
    StringSet.add line set
  in
  let state =
    let end_of_fp = "Should not be detected" ^ PP.style_reset in
    let end_of_fn = "Not detected" ^ PP.style_reset in
    if String.starts_with ~prefix:(Filename.concat "." "examples") line then
      let unique_success_lines =
        add_unique_line state.State.unique_success_lines
      in
      {state with unique_success_lines}
    else if
      String.ends_with ~suffix:end_of_fp line
      || String.ends_with ~suffix:end_of_fn line
    then
      let unique_failure_lines =
        add_unique_line state.State.unique_failure_lines
      in
      {state with unique_failure_lines}
    else state
  in
  let get ~default extract_from =
    Option.value ~default (extract_from line)
  in
  let total =
    get ~default:(Scores.total state.scores) Scores.extract_total
  in
  let failed =
    get ~default:(Scores.failed state.scores) Scores.extract_failed
  in
  State.set_scores total failed state

let process state filepath =
  let input_lines =
    In_channel.with_open_text filepath In_channel.input_lines
  in
  print_title (Filename.remove_extension filepath);
  let local_state = List.fold_left update State.init input_lines in
  Scores.pp local_state.scores;
  Printf.printf "\n";
  State.join local_state state

let () =
  let filepaths =
    let argv_len = Array.length Sys.argv in
    Array.sub Sys.argv 1 (argv_len - 1)
  in
  let state = Array.fold_left process State.init filepaths in

  Printf.printf "%s\n\n" (String.make 16 '.');

  print_res "+-  Summed Results  -+" state.scores;

  let unified_failed = StringSet.cardinal state.unique_failure_lines in
  let unified_success = StringSet.cardinal state.unique_success_lines in
  let unified_scores =
    Scores.set_success unified_success Scores.init
    |> Scores.set_failures unified_failed
  in

  Printf.printf "\n";

  print_res "[>  Unified Results  <]" unified_scores;

  if unified_failed > 0 then
    exit 1
  else if unified_success = 0 then
    (* no test run ? *)
    exit 2

