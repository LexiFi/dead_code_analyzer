type t = {
  cm_file : string;
  sourcepath : string option;
  builddir : string;
  modname : string;
  cmti_infos : Cmt_format.cmt_infos option;
  cmt_infos : Cmt_format.cmt_infos option;
}

let empty = {
  cm_file = "";
  sourcepath = None;
  builddir = "!!UNKNOWN_BUILDDIR!!";
  modname = "!!UNKNOWN_MODNAME!!";
  cmti_infos = None;
  cmt_infos = None;
}

(** [init_from_cmt_infos ~orig ~cm_file cmt_infos] creates a [t] with:
    - information from [cmt_infos];
    - [cm_file];
    - [cmt_infos = Some cmt_infos] if [orig = `Cmt]
    - [cmti_infos = Some cmt_infos] if [orig = `Cmti] *)
  let init_from_cmt_infos ~orig ~cm_file cmt_infos =
  let builddir = cmt_infos.Cmt_format.cmt_builddir in
  let sourcepath =
    Option.map (Filename.concat builddir) cmt_infos.cmt_sourcefile
  in
  let modname = cmt_infos.cmt_modname in
  let cmt_infos, cmti_infos =
    match orig with
    | `Cmt -> Some cmt_infos, None
    | `Cmti -> None, Some cmt_infos
  in
  {cm_file; sourcepath; builddir; modname; cmt_infos; cmti_infos}

(** [init_from_cm_file ~orig cm_file] returns an [Ok t] with [t] filled using
    the [cm_file] (see [init_from_cmt_infos]).
    In case the file does not exist or it cannot be read (see
    [Cmt_format.read_cmt]) then it returns an [Err msg] with msg a string
    describing the issue. *)
let init_from_cm_file ~orig cm_file =
  if not (Sys.file_exists cm_file) then Result.error (cm_file ^ ": file not found")
  else
    try
      let cmt_infos = Cmt_format.read_cmt cm_file in
      init_from_cmt_infos ~orig ~cm_file cmt_infos
      |> Result.ok
    with _ ->
      let ext =
        match orig with
        | `Cmt -> ".cmt"
        | `Cmti -> ".cmti"
      in
      Result.error (Printf.sprintf "%s: cannot read %s file" cm_file ext)

let init cm_file =
  let ( let* ) x f = Result.bind x f in
  let* orig =
    match Filename.extension cm_file with
    | ".cmt" -> Result.ok `Cmt
    | ".cmti" -> Result.ok `Cmti
    | _ -> Result.error (cm_file ^ ": not a .cmti or .cmt file")
  in
  let* file_infos = init_from_cm_file ~orig cm_file in
  let cmt_infos, cmti_infos =
    let no_ext = Filename.remove_extension cm_file in
    match orig with
    | `Cmt ->
        let cmti_infos =
          init_from_cm_file ~orig:`Cmti (no_ext ^ ".cmti")
          |> Result.map (fun {cmti_infos; _} -> cmti_infos)
          |> Result.value ~default:file_infos.cmti_infos
        in
        file_infos.cmt_infos, cmti_infos
    | `Cmti ->
        let cmt_infos =
          init_from_cm_file ~orig:`Cmt (no_ext ^ ".cmt")
          |> Result.map (fun {cmt_infos; _} -> cmt_infos)
          |> Result.value ~default:file_infos.cmt_infos
        in
        cmt_infos, file_infos.cmti_infos
  in
  Result.ok {file_infos with cmt_infos; cmti_infos}

let change_file file_infos cm_file =
  let no_ext = Filename.remove_extension cm_file in
  assert(no_ext = Filename.remove_extension file_infos.cm_file);
  match Filename.extension cm_file, file_infos with
  | ".cmt", {cmt_infos=Some cmt_infos; cmti_infos; _} ->
      let res = init_from_cmt_infos ~orig:`Cmt ~cm_file cmt_infos in
      Result.ok {res with cmti_infos}
  | ".cmti", {cmti_infos=Some cmti_infos; cmt_infos; _} ->
      let res = init_from_cmt_infos ~orig:`Cmti ~cm_file cmti_infos in
      Result.ok {res with cmt_infos}
  | _ -> (* corresponding info is None or invlaid extension *)
      init cm_file

let has_sourcepath file_infos = Option.is_some file_infos.sourcepath

let get_builddir t = t.builddir

let get_sourcepath t =
  match t.sourcepath with
  | Some sourcepath -> sourcepath
  | None ->
      Printf.sprintf "!!UNKNOWN_SOURCEPATH_IN<%s>_FOR_<%s>!!"
        t.builddir
        t.cm_file

let get_sourceunit t =
  match t.sourcepath with
  | Some sourcepath -> Utils.unit sourcepath
  | None -> "!!UNKNOWN_SOURCEUNIT_FOR<" ^ t.cm_file ^ ">!!"

let get_modname t = t.modname
