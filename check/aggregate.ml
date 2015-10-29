let total = ref 0
let err = ref 0
let res = Hashtbl.create 256

let print_title title =
    print_string "\x1b[1;37m";
    print_endline title;
    print_endline (String.make (String.length title) '~');
    print_endline "\x1b[0m"

let rec update file form value =
  try
    let line = input_line file in
    if String.length line > 2 && not (Hashtbl.mem res line) then
      if line.[0] = '.' && line.[1] <> '>' && not (Hashtbl.mem res line) then
        Hashtbl.add res line true
      else if line.[0] = '\x1b' && not (Hashtbl.mem res line) then
        Hashtbl.add res line false;
    try
      let tot = Scanf.sscanf line form (fun _ x _ -> x) in
      print_endline line;
      value := tot + !value
    with
        | _ -> update file form value
  with End_of_file -> close_in file

let rec process n =
  if n = 0 then ()
  else begin
    let file = open_in Sys.argv.(n) in
    print_title (Filename.chop_extension Sys.argv.(n));

    update file "Total: \x1b[0;%dm%d%s" total;
    update file "Failed: \x1b[0;%dm%d%s" err;
    begin try print_endline (input_line file); print_endline "\n\n"
    with _ -> () end;
    process (n - 1)
  end

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
  process (Array.length Sys.argv - 1);
  print_endline "...............................................\n\n\x1b[1;37m";

  print_endline "~~~~~~~~~~~~~~~~~~~~~~";
  print_endline "|                    |";
  print_endline "+-  Summed Results  -+";
  print_res     "|                    |" !total !err;

  let total, err = Hashtbl.fold
    (fun _ valid (total, err) ->
      if valid then (total + 1, err)
      else (total + 1, err + 1))
    res
    (0, 0)
  in

  print_endline "\n\n\x1b[1;37m";

  print_endline "~~~~~~~~~~~~~~~~~~~~~~~";
  print_endline "|                     |";
  print_endline "[>  Unified Results  <]";
  print_res     "|                     |" total err

