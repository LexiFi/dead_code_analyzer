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
  let fn = get_element ~regexp:"[</].*.mli?" line in
  if fn <> "" then String.sub fn 1 @@ String.length fn - 1
  else fn

(* Extract line number name from current line *)
let get_pos line =
  let pos = get_element ~regexp:":[0-9]*:" ~f:Str.search_backward ~start:(String.length line - 1) line in
  int_of_string (if pos <> "" then String.sub pos 1 @@ String.length pos - 2
  else pos)

(* Extract value name from current line (for optional arg) *)
let get_value line =
  let x = get_element ~regexp:" .* " ~f:Str.search_backward ~start:(String.length line - 1) line in
  if x <> "" then String.sub x 1 @@ String.length x - 2
  else x

(* Extract extra info from current line *)
let get_info line =
  get_element ~regexp:" .*[\r\n]?" ~f:Str.search_backward ~start:(String.length line - 1) line

let sec_part ?(regexp = ".*") line =
  Str.string_match (Str.regexp regexp) line 0

let sec_start = sec_part ~regexp:"=+"
let sec_end = sec_part ~regexp:"-+"


(******** Error messages ********)
let errors = ref 0 (* Nb FP/FN *)

let error ?(why = "unknown reason") ?(neg = false) ~where () =
  incr errors;
  prerr_string "\x1b[0;31m";
  prerr_string where;
  prerr_string ": \x1b[0;37;41m";
  prerr_string why;
  prerr_string "\x1b[0m"
  |> prerr_newline

(******** Processing ********)

let total = ref 0 (* Nb tests *)
let comp = ref "" (* Line to compare with *)
let nextl = ref "" (* Line to verify *)
let dir = ref "" (* Directory to find expected outputs *)
let res = ref (open_in_gen [Open_creat] 777 "trash.out") (* Output file computed from analysis on ../examples *)
let fn = ref None (* Filename that should currently be processed *)
let in_file = ref !res (* File fn *)
let old_fn = ref None (* Previous filename processed *)
let extend = ref "" (* section specific extension *)

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
                          nextl := "";
                          false
                    | _ -> fn := Some name;
                          try
                            in_file := open_in @@ !dir ^ name;
                            true
                          with _ ->
                            error ~why:"File not found or cannot be opened."
                                  ~where:(!dir ^ name) ();
                            fn := None;
                            nextl := "";
                            false
                end
      | Some str when str = name ->
          if not (!in_file = (open_in "trash.out")) then true
          else (error ~where:line (); false)
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

let check_elt ~f line x = compare x @@ f line

let check_aux line status=
  if status > 0 then (error ~why:("Not detected") ~where:line (); comp := ""; false)
  else if status < 0 then (error ~why:("Should not be detected") ~where:!nextl (); nextl := ""; false)
  else true

let check_value line x =
  check_elt ~f:get_value line x |> check_aux line

let check_pos line pos =
  check_elt ~f:get_pos line pos |> check_aux line

let check_info line info =
  if (check_elt ~f:get_info line info) <> 0 then
    (error ~why:("Expected:" ^ (get_info line)) ~where:!nextl ();
    nextl := ""; comp := ""; false)
  else true

  (**** Blocks ****)

let rec section ?(fn = true) ?(pos = true) ?(value = false) ?(info = true) () =
  try
    if !nextl = "" then nextl := input_line !res;
    if sec_start !nextl then (nextl := ""; comp := ""; section ~fn ~pos ~value ~info ())
    else if sec_end !nextl then (print_string !nextl; print_string "\n\n\n"; nextl := "")
    else begin
      incr total;
      comp := if fn && !comp = "" then (get_filename !nextl ^ !extend |> check_fn) !nextl else !comp;
      let unit =
        if not fn || !comp <> "" then
          (if not ((pos && not @@ check_pos !comp @@ get_pos !nextl)
              || (value && not @@ check_value !comp @@ get_value !nextl)
              || (info && not @@ check_info !comp @@ get_info !nextl)) then
            (print_endline !nextl; nextl := ""; comp := ""))
      in
      section ~fn ~pos ~value ~info unit
    end
  with End_of_file ->
    let tmp = open_in "trash.out" in
    if !in_file <> tmp then (close_in tmp; empty !in_file)

let rec sel_section () =
  fn := None; old_fn := None;
  nextl := ""; comp := "";
  try
    match (input_line !res) with
        "UNUSED EXPORTED VALUES:" -> print_string "UNUSED EXPORTED VALUES:\n";
            print_string "=======================" |> print_newline
            |> section |> sel_section
      | "UNUSED VALUES:" -> print_string "UNUSED VALUES:\n";
            print_string "=======================" |> print_newline
            |> section |> sel_section
      | "OPTIONAL ARGUMENTS:" -> print_string "OPTIONAL ARGUMENTS:\n";
            print_string "===================" |> print_newline; (extend := "opt")
            |> section ~value:true |> sel_section
      | "CODING STYLE:" -> print_string "CODING STYLE:\n";
            print_string "=============" |> print_newline; (extend := "style")
            |> section |> sel_section
      | _ -> sel_section ()
  with End_of_file -> ()

let result () =
  print_string "Total: \x1b[0;34m";
  print_int !total;
  print_string "\x1b[0m\nFailed: \x1b[0;31m";
  print_int !errors;
  let ratio = ( -. ) 100. @@ ( *. ) 100. @@ (float_of_int !total |> ( /. ) @@ float_of_int !errors) in
  print_string @@ "\x1b[0m\nRatio: \x1b[0;3"
      ^ (if ratio < 50. then "1m" else if ratio < 80. then "3m" else "2m");
  print_float ratio;
  print_string "%\x1b[0m"
  |> print_newline

let () =
  dir :=
    if (Array.length Sys.argv) < 2 then "./"
    else Sys.argv.(1) ^ "/";
  res :=
    if (Array.length Sys.argv) < 3 then open_in "res.out"
    else open_in Sys.argv.(2);
  sel_section () ;
  close_in !res;
  result ()
