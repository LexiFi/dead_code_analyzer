(* Extract an element from a string *)
let get_element line ?(f = Str.search_forward) ?(regexp = ".*") ?(start = 0) =
  try
    f (Str.regexp regexp) line start
    |> ignore;
    Str.matched_string line
  with _ -> ""

(* Extract filename from current line *)
let get_filename = get_element ~regexp:"[</].*\.ml"

(* Extract value name from current line *)
let get_value line =
  get_element ~regexp:" .* " ~f:Str.search_backward ~start:(String.length line) line

(* Extract extra info from current line (for optional arguments) *)
let get_res line =
  get_element ~regexp:" .*[\r\n]" ~f:Str.search_backward ~start:(String.length line) line

let process file dir =
  try
    match (input_line file) with
        "UNUSED EXPORTED VALUES:" -> ()
      | "OPTIONAL ARGUMENTS:" -> ()
      | "CODING STYLE:" -> ()
  with End_of_file -> ()


let () =
  (* Directory to find expected outputs *)
  let dir =
    if (Array.length Sys.argv) < 2 then "."
    else Sys.argv.(1) in
  (* Output file computed from analysis on ../examples *)
  let file =
    if (Array.length Sys.argv) < 3 then open_in "./res.out"
    else open_in Sys.argv.(2) in
  process file dir
