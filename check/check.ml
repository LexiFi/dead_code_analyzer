(******** Identification ********)

(* Extract an element from a string *)
let get_element ?(f = Str.search_forward) ?(regexp = ".*") ?(start = 0) line =
  try
    f (Str.regexp regexp) line start
    |> ignore;
    Str.matched_string line
  with _ -> ""

(* Extract filename from current line *)
let get_filename line =
  let fn = get_element ~regexp:"[</].*\\.mli?" line in
  String.sub fn 1 @@ String.length fn - 1

(* Extract line number name from current line *)
let get_pos line =
  let pos = get_element ~regexp:":.*:" ~f:Str.search_backward ~start:(String.length line - 1) line in
  String.sub pos 1 @@ String.length pos - 2

(* Extract value name from current line *)
let get_value line =
  let x = get_element ~regexp:" .* " ~f:Str.search_backward ~start:(String.length line - 1) line in
  String.sub x 1 @@ String.length x - 2

(* Extract extra info from current line (for optional arguments) *)
let get_info line =
  get_element ~regexp:" .*[\r\n]" ~f:Str.search_backward ~start:(String.length line - 1) line

let sec_part ?(regexp = ".*") line =
  Str.string_match (Str.regexp regexp) line 0

let sec_start = sec_part ~regexp:"=+"
let sec_end = sec_part ~regexp:"-+"


(******** Error messages ********)
let error ?(why = "unknown reason") ~where () =
  print_string where;
  print_string ": ";
  print_string why
  |> print_newline

(******** Processing ********)

let dir = ref "" (* Directory to find expected outputs *)
let res = ref stdin (* Output file computed from analysis on ../examples *)
let fn = ref None (* Filename that should currently be processed *)
let in_file = ref stdin (* File fn *)
let old_fn = ref None (* Previous filename processed *)

let rec empty file =
  try
    let where = input_line file in
    error ~why:"Not detected" ~where ();
    empty file
  with End_of_file -> ()


  (**** Checkers ****)

let rec check_fn name line =
  let ok =
    match !fn with
        None -> begin
                  match !old_fn with
                      Some str when str = name ->
                          error ~why:"Should not be detected" ~where:line ();
                          false
                    | _ -> fn := Some name;
                          try
                            in_file := open_in @@ !dir ^ name;
                            true
                          with _ ->
                            error ~why:"File not found" ~where:name ();
                            false
                end
      | Some str when str = name ->
          if not (!in_file = stdin) then true
          else begin
            error ~where:line ();
            false
          end
      | _ ->
          if in_channel_length !in_file - 1 <= pos_in !in_file then true
          else begin
            empty !in_file;
            close_in !in_file;
            old_fn := !fn;
            fn := None;
            false
          end
  in
  if ok then
    try
      input_line !in_file
    with End_of_file ->
      close_in !in_file;
      old_fn := !fn;
      fn := None;
      check_fn name line
  else ""

let check_elt ?(f = fun x -> x) line x = f line = x

let check_value = check_elt ~f:get_value
let check_pos = check_elt ~f:get_pos
let check_info = check_elt ~f:get_info

  (**** Blocks ****)

let rec section ?(fn = true) ?(pos = true) ?(value = true) ?(info = false) () =
  try
    let line = (input_line !res) in
    if sec_start line then section ~fn ~pos ~value ~info ()
    else if sec_end line then ()
    else let comp = if fn then (get_filename line |> check_fn) line else "" in
      if not fn || comp <> "" then
        if (pos && not @@ check_pos comp @@ get_pos line)
            || (value && not @@ check_value comp @@ get_value line)
            || (info && not @@ check_info comp @@ get_info line) then print_string line
  with End_of_file -> ()

let rec sel_section () =
  fn := None;
  old_fn := None;
  try
    match (input_line !res) with
        "UNUSED EXPORTED VALUES:" -> print_string "UNUSED EXPORTED VALUES:\n";
            print_string "=======================\n";
            section ()
      | "OPTIONAL ARGUMENTS:" -> print_string "OPTIONAL ARGUMENTS:\n";
            print_string "===================\n";
            section ~info:true ()
      | "CODING STYLE:" -> print_string "CODING STYLE:\n";
            print_string "=============\n";
            section ()
      | _ -> sel_section ()
  with End_of_file -> ()


let () =
  dir :=
    if (Array.length Sys.argv) < 2 then "./"
    else Sys.argv.(1);
  res :=
    if (Array.length Sys.argv) < 3 then open_in "./res.out"
    else open_in Sys.argv.(2);
  sel_section () ;
  close_in !res
