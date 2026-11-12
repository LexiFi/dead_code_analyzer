let normalize ~sep path =
  let splitted_path =
    String.split_on_char '\\' path
    |> List.concat_map (String.split_on_char '/')
  in
  let is_drive_name drive =
    String.length drive = 2
    && drive.[0] >= 'A' && drive.[0] <= 'Z'
    && drive.[1] = ':'
  in
  let head, splitted_path =
    match splitted_path with
    | ""::splitted_path when Sys.unix->
      (* do not change Unix aboslute paths *)
      "", splitted_path
    | drive::splitted_path when Sys.win32 && is_drive_name drive ->
      (* do not change Windows absolute paths *)
      drive, splitted_path
    | ""::"cygdrive"::drive::splitted_path when Sys.win32 ->
      (* convert "/cygdrive/D/..." cygwin path into "D:\..." Windows path *)
      let drive = String.uppercase_ascii drive ^ ":" in
      drive, splitted_path
    | _ ->
      (* normalize relative path to start with "./" *)
      ".", splitted_path
  in
  splitted_path
  |> List.filter (fun s -> s <> "" && s <> ".") (* remove redundancies *)
  |> List.cons head
  |> String.concat sep

let normalize_to_unix path =
  normalize ~sep:"/" path

let normalize path =
  normalize ~sep:Filename.dir_sep path

 (* Relocate a `path` found in a `.got` file as a relative path from the
    project's root.
    The tests directories are 'docs', 'using_make, and 'using_dune', located in
    '<project_root>'/examples/'.
    Thus, the relocation is done by extracting the path from the first of those
    directories and prepending it with './examples/`.
    This assumes the '<project_root>' does not contain a directory with one of
    aforementionned name.
    For files in 'using_dune', the path is 'using_dune/_build/default'.
    The '_build/default' is removed. *)
let relocate path =
  (* retrieve the subpath starting from the first occurence of
     a valid basename if it exists, and remove the "_build/default" part
     of the path *)
  let valid_basenames = ["docs"; "using_make"; "using_dune"] in
  let dir_sep =
    if String.length Filename.dir_sep = 1 then Filename.dir_sep.[0]
    else invalid_arg ("Unknown dir sep " ^ Filename.dir_sep)
  in
  let splitted = String.split_on_char dir_sep path in
  let rec extract_subpath = function
    | [] -> splitted
    | basename::_ as splitted when List.mem basename valid_basenames -> splitted
    | _::splitted -> extract_subpath splitted
  in
  let rec remove_build_default acc = function
    | [] -> List.rev acc
    | "_build"::"default"::splitted -> List.rev_append acc splitted
    | basename::splitted -> remove_build_default (basename::acc) splitted
  in
  let relative_path = (* [valid_basename; <path/to/file>...] *)
    extract_subpath splitted
    |> remove_build_default []
  in
  String.concat Filename.dir_sep ("."::"examples"::relative_path)
