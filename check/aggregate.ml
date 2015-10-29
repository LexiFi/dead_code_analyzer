let total = ref 0
let err = ref 0


let rec update file form value =
  try
    let line = input_line file in
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
    print_string "\x1b[1;37m";
    print_endline (Filename.chop_extension Sys.argv.(n));
    print_endline (String.make (String.length Sys.argv.(n)) '~');
    print_endline "\x1b[0m";

    seek_in file (max (in_channel_length file - 200) (pos_in file));

    update file "Total: \x1b[0;%dm%d%s" total;
    update file "Failed: \x1b[0;%dm%d%s" err;
    begin try print_endline (input_line file); print_endline "\n\n"
    with _ -> () end;
    process (n - 1)
  end

let () =
  process (Array.length Sys.argv - 1);
  print_string "\x1b[1;37m";
  print_endline "-----  RESULTS:  -----";
  print_endline "~~~~~~~~~~~~~~~~~~~~~~";
  print_endline "\x1b[0m";
  print_string "Total: \x1b[0;34m";
  print_int !total;
  print_string "\x1b[0m\nFailed: \x1b[0;31m";
  print_int !err;
  let ratio = ( -. ) 100. @@ ( *. ) 100. @@ (float_of_int !total |> ( /. ) @@ float_of_int !err) in
  print_string @@ "\x1b[0m\nRatio: \x1b[0;3"
      ^ (if ratio < 50. then "1m" else if ratio < 80. then "3m" else "2m");
  print_float ratio;
  print_endline "%\x1b[0m"
