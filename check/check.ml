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
  if fn <> "" then String.sub fn 1 @@ String.length fn - 1
  else fn

(* Extract line number name from current line *)
let get_pos line =
  let pos = get_element ~regexp:":.*:" ~f:Str.search_backward ~start:(String.length line - 1) line in
  if pos <> "" then String.sub pos 1 @@ String.length pos - 2
  else pos

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
let error ?(why = "unknown reason") ~where () =
  prerr_string where;
  prerr_string ": ";
  prerr_string why
  |> prerr_newline

(******** Processing ********)

let dir = ref "" (* Directory to find expected outputs *)
let res = ref (open_in_gen [Open_creat] 777 "trash.out") (* Output file computed from analysis on ../examples *)
let fn = ref None (* Filename that should currently be processed *)
let in_file = ref !res (* File fn *)
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
                            error ~why:"File not found or cannot be opened"
                                  ~where:(!dir ^ name) ();
                            fn := None;
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

let check_elt ?(f = fun x -> x) line x = f line = x

let check_value line x =
  if not (check_elt ~f:get_value line x) then (error ~why:("Got value: " ^ x) ~where:line; false)
  else true

let check_pos line pos = 
  if not (check_elt ~f:get_pos line pos) then (error ~why:("Got position: " ^ pos) ~where:line; false)
  else true

let check_info line info = 
  if not (check_elt ~f:get_info line info) then (error ~why:("Got information: " ^ info) ~where:line; false)
  else true

  (**** Blocks ****)

let rec section ?(fn = true) ?(pos = true) ?(value = false) ?(info = true) () =
  try
    let line = (input_line !res) in
    if sec_start line then section ~fn ~pos ~value ~info ()
    else if sec_end line then (print_string line; print_string "\n\n\n")
    else let comp = if fn then (get_filename line |> check_fn) line else "" in
      let unit =
        if not fn || comp <> "" then
          (if not ((pos && not @@ check_pos comp @@ get_pos line)
            || (value && not @@ check_value comp @@ get_value line)
            || (info && not @@ check_info comp @@ get_info line)) then print_string line |> print_newline
          else print_newline ())
        else (error ~why:"don't know" ~where:line |> ignore)
      in
      section ~fn ~pos ~value ~info unit
  with End_of_file -> ()

let rec sel_section () =
  fn := None;
  old_fn := None;
  try
    match (input_line !res) with
        "UNUSED EXPORTED VALUES:" -> print_string "UNUSED EXPORTED VALUES:\n";
            print_string "=======================\n"
            |> section |> sel_section
      | "OPTIONAL ARGUMENTS:" -> print_string "OPTIONAL ARGUMENTS:\n";
            print_string "===================\n"
            |> section ~value:true |> sel_section
      | "CODING STYLE:" -> print_string "CODING STYLE:\n";
            print_string "=============\n"
            |> section |> sel_section
      | _ -> sel_section ()
  with End_of_file -> ()


let () =
  dir :=
    if (Array.length Sys.argv) < 2 then "./"
    else Sys.argv.(1) ^ "/";
  res :=
    if (Array.length Sys.argv) < 3 then open_in "res.out"
    else open_in Sys.argv.(2);
  sel_section () ;
  print_string ("dir: " ^ !dir ^"\n");
  close_in !res
