                (********   IDENTIFICATION  ********)

(* Extract an element from a string *)
let get_element ?(f = Str.search_forward) ~regexp ?(start = 0) line =
  try
    f (Str.regexp regexp) line start
    |> ignore;
    Str.matched_string line
  with _ -> ""

(* Extract abs_path from current line *)
let get_path =
  get_element ~regexp:"\\./.*[</].*.mli?"

(* Extract line number name from current line *)
let get_pos line =
  let pos = get_element ~regexp:":[0-9]*:" line in
  int_of_string (if pos <> "" then String.sub pos 1 @@ String.length pos - 2
  else pos)

(* Extract extra info from current line *)
let get_info line =
  let info =
    get_element ~regexp:": .*$" ~f:Str.search_backward ~start:(String.length line - 1) line
  in
  String.sub info 1 (String.length info - 1)

let sec_part ~regexp line =
  Str.string_match (Str.regexp regexp) line 0

let sec_start = sec_part ~regexp:" *=+"
let sec_end = sec_part ~regexp:"-+\\|\\(Nothing else to report in this section\\)"


                (********   ERROR HANDLING   ********)

let errors = ref 0 (* Nb FP/FN *)

let error ~why ~where () =
  incr errors;
  prerr_string "\x1b[0;31m";
  prerr_string where;
  prerr_string ": \x1b[0;37;41m";
  prerr_string why;
  prerr_string "\x1b[0m"
  |> prerr_newline

                (********   ATTRIBUTES   ********)

let total = ref 0               (* nb tests *)

let comp = ref ""               (* line to compare with *)
let nextl = ref ""              (* line to verify *)

let fnames = ref []
let dir = ref ""                (* directory to find expected outputs *)
let res = ref (open_in_gen [Open_creat] 777 "trash.out")  (* output file computed from analysis on ../examples *)

let fn = ref None               (* filename that should currently be processed *)
let in_file = ref !res          (* file fn *)
let old_fn = ref None           (* previous filename processed *)

let extend = ref ""             (* section specific extension *)

                (********   HELPERS   ********)

let normalize fname =
  let rec block fname pos len =
    if pos = String.length fname || fname.[pos] = '/' then
      String.sub fname (pos - len) len
    else block fname (pos + 1) (len + 1)
  in
  let rec normalize acc fname pos =
    if pos = String.length fname then acc
    else
      let blk = block fname pos 0 in
      if blk = "" || blk = "." then
        normalize acc fname (pos + 1)
      else
        normalize (acc ^ "/" ^ blk) fname (pos + String.length blk)
  in normalize "." fname 0

(* Prints all unreported lines from file *)
let rec empty file =
  try
    let where = input_line file in
    if where <> "" then (error ~why:"Not detected" ~where (); incr total);
    empty file
  with _ -> close_in file

(* Empty all files until one respect the condition *)
let rec empty_fnames ?(regexp = ".*") threshold = function
  | e::l ->
      let extension s =
        let tmp = Filename.chop_extension s in
        String.sub s (String.length tmp) (String.length s - String.length tmp)
      in
      if (try extension e <= extension threshold with _ -> true)
      && get_element ~regexp ~start:0 e < threshold then (
        empty @@ open_in e;
        empty_fnames ~regexp threshold l)
      else e::l
  | _ -> []

let is_trash diff eq =
  let tmp = open_in "trash.out" in
  if !in_file <> tmp then (close_in tmp; diff ())
  else eq

        (**** Checkers ****)

(* Filename *)
let rec check_fn name line =

  let ok = match !fn with
    | None -> begin match !old_fn with
      | Some str when str = name ->
          decr total;
          error ~why:"Should not be detected" ~where:line ();
          nextl := "";
          false
      | _ -> fn := Some name;
          let name = normalize (!dir ^ name) in
          if (try Filename.chop_extension name >= Filename.chop_extension (List.hd !fnames) with _ -> false) then
            fnames := List.tl (empty_fnames name !fnames);
          try
            empty !in_file;
            in_file := open_in name;
            true
          with _ ->
            error ~why:"File not found or cannot be opened."
                  ~where:name
                  ();
            fn := None;
            nextl := "";
            false
      end
    | Some str when str = name -> is_trash (fun () -> true) false
    | _ ->
        if in_channel_length !in_file - 1 <= pos_in !in_file then true
        else begin
          empty !in_file;
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
  else if status < 0 then (decr total; error ~why:("Should not be detected") ~where:!nextl (); nextl := ""; false)
  else true

let check_value line x =
  check_elt ~f:get_info line x |> check_aux line

let check_pos line pos =
  check_elt ~f:get_pos line pos |> check_aux line

let check_info line info =
  if (check_elt ~f:get_info line info) <> 0 then
    (error ~why:("Expected:" ^ (get_info line)) ~where:!nextl ();
    nextl := ""; comp := ""; false)
  else true

  (**** Blocks ****)

let rec section ?(path = true) ?(pos = true) ?(value = false) ?(info = true) () =
  try
    if !nextl = "" then (nextl := input_line !res; section ~path ~pos ~value ~info ())
    else if sec_start !nextl then (nextl := ""; comp := ""; section ~path ~pos ~value ~info ())
    else if sec_end !nextl then
      (is_trash (fun () -> empty !in_file) ();
      print_string !nextl; print_string "\n\n\n"; nextl := "")
    else if !comp <> "" && normalize (get_path !comp) <> normalize (get_path !nextl) then begin
      empty !in_file;
      comp := "";
      old_fn := !fn;
      fn := None;
      section ~path ~pos ~value ~info ()
    end
    else begin
      incr total;
      comp := if path && !comp = "" then ((Filename.chop_extension @@ get_path !nextl) ^ !extend |> check_fn) !nextl else !comp;
      if not path || !comp <> "" then
        if not ((pos && not @@ check_pos !comp @@ get_pos !nextl)
        || (value && not @@ check_value !comp @@ get_info !nextl)
        || (info && not @@ check_info !comp @@ get_info !nextl)) then
          (print_endline !nextl; nextl := ""; comp := "");
      section ~path ~pos ~value ~info ()
    end
  with End_of_file -> is_trash (fun () -> empty !in_file) ()

let rec sel_section () =
  fn := None; old_fn := None;
  nextl := ""; comp := "";
  try
    match (input_line !res) with
      | ".> UNUSED EXPORTED VALUES:" as s ->
            (try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" ".mli" !fnames
            with _ -> ());
            print_endline s;
            print_endline (input_line !res);
            extend := ".mli";
            sel_section (section ())
      | ".> UNUSED CLASS FIELDS:" as s ->
            (try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" ".mlio" !fnames
            with _ -> ());
            print_endline s;
            print_endline (input_line !res);
            extend := ".mlio";
            sel_section (section ())
      | ".> UNUSED TYPES FIELDS/CONSTRUCTORS:" as s ->
            (try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" ".mlit" !fnames
            with _ -> ());
            print_endline s;
            print_endline (input_line !res);
            extend := ".mlit";
            sel_section (section ())
      | ".> OPTIONAL ARGUMENTS: ALWAYS:" as s ->
            (try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" ".mlopta" !fnames
            with _ -> ());
            print_endline s;
            print_endline (input_line !res);
            extend := ".mlopta";
            sel_section (section ~value:true ~info:false ())
      | ".> OPTIONAL ARGUMENTS: NEVER:" as s ->
            (try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" ".mloptn" !fnames
            with _ -> ());
            print_endline s;
            print_endline (input_line !res);
            extend := ".mloptn";
            sel_section (section ~value:true ~info:false ())
      | ".> CODING STYLE:" as s ->
            (try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" ".mlstyle" !fnames
            with _ -> ());
            print_endline s;
            print_endline (input_line !res);
            extend := ".mlstyle";
            sel_section (section ())
      | s when String.length s > 36 && String.sub s 0 36 = ".>->  ALMOST UNUSED EXPORTED VALUES:" ->
            let n =
              Scanf.sscanf s ".>->  ALMOST UNUSED EXPORTED VALUES: Called %s time(s)" (fun n -> n)
            in
            begin try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" (".mli" ^ n) !fnames
            with _ -> () end;
            print_endline s;
            print_endline (input_line !res);
            extend := ".mli" ^ n;
            sel_section (section ())
      | s when String.length s > 33 && String.sub s 0 33 = ".>->  ALMOST UNUSED CLASS FIELDS:" ->
            let n =
              Scanf.sscanf s ".>->  ALMOST UNUSED CLASS FIELDS: Called %s time(s)" (fun n -> n)
            in
            begin try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" (".mlio" ^ n) !fnames
            with _ -> () end;
            print_endline s;
            print_endline (input_line !res);
            extend := ".mlio" ^ n;
            sel_section (section ())
      | s when String.length s > 55 && String.sub s 0 46 = ".>->  ALMOST UNUSED TYPES FIELDS/CONSTRUCTORS:" ->
            let n =
              Scanf.sscanf s ".>->  ALMOST UNUSED TYPES FIELDS/CONSTRUCTORS: Called %s time(s)" (fun n -> n)
            in
            begin try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" (".mlit" ^ n) !fnames
            with _ -> () end;
            print_endline s;
            print_endline (input_line !res);
            extend := ".mlit" ^ n;
            sel_section (section ())
      | s when String.length s > 40 && String.sub s 0 40 = ".>->  OPTIONAL ARGUMENTS: ALMOST ALWAYS:" ->
            let n =
              Scanf.sscanf s ".>->  OPTIONAL ARGUMENTS: ALMOST ALWAYS: Except %s time(s)" (fun n -> n)
            in
            begin try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" (".mlopta" ^ n) !fnames
            with _ -> () end;
            print_endline s;
            print_endline (input_line !res);
            extend := ".mlopta" ^ n;
            sel_section (section ())
      | s when String.length s > 39 && String.sub s 0 39 = ".>->  OPTIONAL ARGUMENTS: ALMOST NEVER:" ->
            let n =
              Scanf.sscanf s ".>->  OPTIONAL ARGUMENTS: ALMOST NEVER: Except %s time(s)" (fun n -> n)
            in
            begin try fnames := empty_fnames ~regexp:"\\.ml[a-z0-9]*$" (".mloptn" ^ n) !fnames
            with _ -> () end;
            print_endline s;
            print_endline (input_line !res);
            extend := ".mloptn" ^ n;
            sel_section (section ())
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
  print_endline "%\x1b[0m"

let rec get_fnames ?(acc = []) dir =
  try
    if Sys.is_directory dir then
      acc @ Array.fold_left (fun l s -> get_fnames ~acc:l (normalize (dir ^ "/" ^ s))) [] @@ Sys.readdir dir
    else if dir <> "./check.ml" && Str.string_match (Str.regexp ".*/[_a-zA-Z0-9-]*.ml[a-z0-9]*") dir 0 then dir::acc
    else acc
  with _ -> acc

let () =
  dir :=
    if (Array.length Sys.argv) < 2 then "."
    else Sys.argv.(1);
  res :=
    if (Array.length Sys.argv) < 3 then open_in "res.out"
    else open_in Sys.argv.(2);
  fnames := List.fast_sort
    (fun x y ->
      let req s =
        get_element ~f:Str.search_backward ~regexp:"\\.ml[a-z0-9]*" ~start:(String.length s - 1) s in
      let c = compare (req x) (req y) in
      if c = 0 then compare x y
      else c)
    @@ get_fnames !dir;
  dir := !dir ^ "/";
  sel_section () ;
  close_in !res;
  result ()
