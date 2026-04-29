let usage () =
  Printf.eprintf "Usage: ocaml %s <bench_dir1> <bench_dir2>\n" Sys.argv.(0)

type errors =
  | File_not_found of string
  | Not_a_file of string

let validate_file filepath =
  if not (Sys.file_exists filepath) then
    Result.error (File_not_found filepath)
  else if not (Sys.is_regular_file filepath) then
    Result.error (Not_a_file filepath)
  else
    Result.ok filepath

let extract_value ~prefix line =
  if String.starts_with ~prefix line then
    let len_prefix = String.length prefix in
    let value = String.sub line len_prefix (String.length line - len_prefix) in
    Some value
  else None

let float_of_time time =
  String.split_on_char ':' time
  |> List.fold_left (fun acc s -> Float.fma acc 60. (float_of_string s)) 0.

let float_of_mem mem = float_of_string mem

let get_time_and_memory file =
  let lines = In_channel.input_lines file in
  let time_prefix = "\tElapsed (wall clock) time (h:mm:ss or m:ss): " in
  let mem_prefix = "\tMaximum resident set size (kbytes): " in
  List.fold_left (fun (time, memory) line ->
      match time, memory with
      | Some _, Some _ -> time, memory
      | None, _ ->
          let time = extract_value ~prefix:time_prefix line in
          let time = Option.map float_of_time time in
          time, memory
      | Some _, None ->
          let memory = extract_value ~prefix:mem_prefix line in
          let memory = Option.map float_of_mem memory in
          time, memory
    )
    (None, None)
    lines

let print_stats ~category metric1 metric2 =
  let absolute_diff = metric2 -. metric1 in
  let relative_diff = absolute_diff /. metric1 *. 100. in
  Printf.printf "%s: %F, %F, %F, %F%%\n" category metric1 metric2 absolute_diff relative_diff

let compare_files ~category bench_file1 bench_file2 =
  match In_channel.with_open_text bench_file1 get_time_and_memory with
  | None, _ | _, None -> ()
  | Some time1, Some mem1 ->
      match In_channel.with_open_text bench_file2 get_time_and_memory with
      | None, _ | _, None -> ()
      | Some time2, Some mem2 ->
        print_stats ~category:(category ^ " (time)") time1 time2;
        print_stats ~category:(category ^ " (memory)") mem1 mem2


let compare_dirs bench_dir1 bench_dir2 =
  let files = Sys.readdir bench_dir1 in
  Array.iter (fun filename ->
      if not (Filename.extension filename = ".err") then ()
      else
        match Filename.concat bench_dir1 filename |> validate_file with
        | Result.Error _ -> ()
        | Result.Ok bench_file1 ->
            match Filename.concat bench_dir2 filename |> validate_file with
              | Result.Error _ -> ()
              | Result.Ok bench_file2 ->
                  let category = Filename.remove_extension filename in
                  compare_files ~category bench_file1 bench_file2
    )
    files


let get_bench_dir arg_pos =
  let bench_dir = Sys.argv.(arg_pos) in
  if not (Sys.file_exists bench_dir) then
    invalid_arg (bench_dir ^ ": directory does not exist");
  if not (Sys.is_directory bench_dir) then
    invalid_arg (bench_dir ^ ": is not a directory");
  bench_dir

let () =
  if Array.length Sys.argv < 3 then (
    usage ();
    exit 1
  );
  let bench_dir1 = get_bench_dir 1 in
  let bench_dir2 = get_bench_dir 2 in
  compare_dirs bench_dir1 bench_dir2
