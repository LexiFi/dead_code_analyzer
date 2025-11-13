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
    - For files in 'using_make': there is no 'using_make' file or directory in
   '<project_root>/examples/using_<dune|make>'. Therefore, taking everything in the
   path from the last 'using_make' filename provides the common part for the
   relocation. Then, adding an extra './' completes the path as found in the
   expected reports
 - For files in 'using_dune': the same startegy as for files in 'using_make'
   can be applied with the removal of '_build/default' on the way.
   Because those directory names are unique, we can actually share the same
   logic for both 'using_make' and 'using_dune'. *)
let relocate path =
  (* retrieve the subpath starting from the last occurence of
     "using_<make|dune>" if it exists, and remove "_build" and "default"
     directories from the path *)
  let rec extract_subpath acc dirpath =
    let basename = Filename.basename dirpath in
    if basename = "using_make" || basename = "using_dune" then
      basename::acc
    else if basename = dirpath (* fixpoint *) then
      path::[] (* TODO: error handling *)
    else
      let acc =
        match basename with
        | "_build" | "default" -> acc
        | basename -> basename::acc
      in
      extract_subpath acc (Filename.dirname dirpath)
  in
  let relative_path = (* ["using<make|dune>"; <path/to/file>...] *)
    extract_subpath [] path
  in
  String.concat Filename.dir_sep ("."::"examples"::relative_path)
