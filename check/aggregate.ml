let total = ref 0
let err = ref 0
let unified_res = Hashtbl.create 256
let sec_res = Hashtbl.create 256

let print_title title =
  print_string "\x1b[1;37m";
  print_endline title;
  print_endline (String.make (String.length title) '~');
  print_endline "\x1b[0m"

let update ~total_fmt ~failed_fmt line =
  let unique_line =
    let rec make_unique_to_sec n =
      let new_line = line ^ string_of_int n in
      if Hashtbl.mem sec_res new_line then make_unique_to_sec (n + 1)
      else new_line
    in
    let res = make_unique_to_sec 0 in
    Hashtbl.add sec_res res ();
    res
  in
  if not (Hashtbl.mem unified_res unique_line) then (
    if String.starts_with ~prefix:"./examples" line then
      Hashtbl.add unified_res unique_line true
    else if
      String.ends_with ~suffix:"Should not be detected\x1b[0m" line
      || String.ends_with ~suffix:"Not detected\x1b[0m" line
    then
      Hashtbl.add unified_res unique_line false
  );
  let update_count fmt value =
    try
      let fmt = Scanf.format_from_string fmt "%d%d%s" in
      let tot = Scanf.sscanf line fmt (fun _ x _ -> x) in
      print_endline line;
      value := tot + !value
    with _ -> ()
  in
  update_count total_fmt total;
  update_count failed_fmt err

let process filepath =
  let input_lines =
    In_channel.with_open_text filepath In_channel.input_lines
  in
  print_title (Filename.remove_extension filepath);

  let total_fmt = "Total: \x1b[%dm%d%s" in
  let failed_fmt = "Failed: \x1b[%dm%d%s" in
  List.iter (update ~total_fmt ~failed_fmt) input_lines;
  Hashtbl.clear sec_res;
  print_endline "\n"

let print_res title total err =
  print_title title;
  print_string "Total: \x1b[0;34m";
  print_int total;
  print_string "\x1b[0m\nFailed: \x1b[0;31m";
  print_int err;
  let ratio = ( -. ) 100. @@ ( *. ) 100. @@ (float_of_int total |> ( /. ) @@ float_of_int err) in
  print_string @@ "\x1b[0m\nRatio: \x1b[0;3"
                  ^ (if ratio < 50. then "1m" else if ratio < 80. then "3m" else "2m");
  print_float ratio;
  print_endline "%\x1b[0m"

let () =
  let filepaths =
    let argv_len = Array.length Sys.argv in
    Array.sub Sys.argv 1 (argv_len - 1)
  in
  Array.iter process filepaths;
  print_endline "...............................................\n\n\x1b[1;37m";

  print_endline "~~~~~~~~~~~~~~~~~~~~~~";
  print_endline "|                    |";
  print_endline "+-  Summed Results  -+";
  print_res     "|                    |" !total !err;

  let total, err = Hashtbl.fold
      (fun _ valid (total, err) ->
         if valid then (total + 1, err)
         else (total + 1, err + 1))
      unified_res
      (0, 0)
  in

  print_endline "\n\n\x1b[1;37m";

  print_endline "~~~~~~~~~~~~~~~~~~~~~~~";
  print_endline "|                     |";
  print_endline "[>  Unified Results  <]";
  print_res     "|                     |" total err;

  if err > 0 then
    exit 1

