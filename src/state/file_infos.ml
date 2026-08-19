type annots =
  | Structure of Typedtree.structure
  | Signature of Typedtree.signature
  | Both of { sign: Typedtree.signature; strc: Typedtree.structure }
  | Neither

let merge_annots sign strc =
  match sign, strc with
  | Structure _, _ -> Result.error "expected a signature first"
  | _, Signature _ -> Result.error "expected a structure second"
  | Neither, annots | annots, Neither -> Result.ok annots
  | (Signature sign | Both {sign; _}), (Structure strc | Both {strc; _}) ->
      Result.ok (Both {sign; strc})

type t = {
  builddir : string;
  cm_file : string;
  annots : annots;
  cmti_uid_to_decl : Location_dependencies.uid_to_decl option;
  location_dependencies : Location_dependencies.t;
  modname : string;
  sourcepath : string option;
}

let empty = {
  builddir = "!!UNKNOWN_BUILDDIR!!";
  cm_file = "";
  annots = Neither;
  cmti_uid_to_decl = None;
  location_dependencies = Location_dependencies.empty;
  modname = "!!UNKNOWN_MODNAME!!";
  sourcepath = None;
}

(** [init_from_all_cm_infos ~cm_file cmt_infos] creates a [t] with:
    - information from [cmt_infos] : [builddir], [modname], [sourcepath];
    - [cm_file];
    - [annots] is extracted from [cmt_infos.cmt_annots]
*)
let init_from_all_cm_infos ~cm_file cmt_infos =
  let builddir = cmt_infos.Cmt_format.cmt_builddir in
  let sourcepath =
    Option.map Utils.Filepath.remove_pp cmt_infos.cmt_sourcefile
    |> Option.map (Filename.concat builddir)
  in
  let modname = cmt_infos.cmt_modname in
  let annots =
    match cmt_infos.cmt_annots with
    | Interface sign -> Signature sign
    | Implementation strc -> Structure strc
    | _ -> Neither
  in
  {empty with builddir;
              cm_file;
              annots;
              modname;
              sourcepath}

(** [init_from_cm_file cm_file] returns an [Ok t] with [t] filled with general
    info expected for both cmt and cmti files, using the [cm_file] (see
    [init_from_all_cm_infos]).
    In case the file does not exist or it cannot be read (see
    [Cmt_format.read_cmt]) then it returns an [Err msg] with msg a string
    describing the issue. *)
let init_from_cm_file cm_file =
  if not (Sys.file_exists cm_file) then Result.error (cm_file ^ ": file not found")
  else
    match Cmt.read cm_file with
    | Error _ as err -> err
    | Ok (_, cmt_infos) ->
        let file_infos =
          init_from_all_cm_infos ~cm_file cmt_infos
        in
        Result.ok (file_infos, cmt_infos)

let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x

let init_from_cmti_file cmti_file =
  let* file_infos, cmt_infos = init_from_cm_file cmti_file in
  let cmti_uid_to_decl = Some cmt_infos.cmt_uid_to_decl in
  match file_infos.annots with
  | Signature _ | Both _ ->
      let file_infos = {file_infos with cmti_uid_to_decl} in
      Result.ok file_infos
  | _ -> Result.error (cmti_file ^ ": does not contain an interface")

let init_from_cmt_file ~comp_unit_to_path cmt_file =
  let* file_infos, cmt_infos = init_from_cm_file cmt_file in
  let* annots =
    match file_infos.annots with
    | Structure _ | Both _ -> Result.ok file_infos.annots
    | _ -> Result.error (cmt_file ^ ": does not contain an implementation")
  in
  (* Read the cmti if it exists. We always want to do it in case a user
     specified the cmt before the cmti to ensure the location_dependencies
     are idempotent. *)
  let cmti_uid_to_decl, annots =
    let cmti_file = Filename.remove_extension cmt_file ^ ".cmti" in
    match init_from_cmti_file cmti_file with
    | Error _ -> None, annots
    | Ok file_infos ->
        let annots =
          match merge_annots file_infos.annots annots with
          | Error _ ->
              (* TODO: better error handling *)
              assert false
          | Ok annots -> annots
        in
        file_infos.cmti_uid_to_decl, annots
  in
  let+ location_dependencies =
    Location_dependencies.init ~comp_unit_to_path cmt_infos cmti_uid_to_decl
  in
  let file_infos =
    {file_infos with annots; cmti_uid_to_decl; location_dependencies}
  in
  file_infos, cmt_infos

let init ~comp_unit_to_path cm_file =
  match Filename.extension cm_file with
  | ".cmt" ->
      let+ file_infos, _ = init_from_cmt_file ~comp_unit_to_path cm_file in
      file_infos
  | ".cmti" -> (
      (* Using cmt_infos is not critical. The intent is to mirror the behavior
         on cmt files, where both cmt and cmti are read. *)
      let filled_with_cmt_infos =
        let cmt_file = Filename.remove_extension cm_file ^ ".cmt" in
        let* file_infos, cmt_infos = init_from_cmt_file ~comp_unit_to_path cmt_file in
        let+ location_dependencies =
          Location_dependencies.init ~comp_unit_to_path cmt_infos file_infos.cmti_uid_to_decl
        in
        {file_infos with location_dependencies}
      in
      match filled_with_cmt_infos with
      | Ok {annots; cmti_uid_to_decl; location_dependencies; _} ->
          let+ res, _ = init_from_cm_file cm_file in
          {res with annots; cmti_uid_to_decl; location_dependencies}
      | Error _ -> init_from_cmti_file cm_file
  )
  | _ -> Result.error (cm_file ^ ": not a .cmti or .cmt file")

let change_file ~comp_unit_to_path file_infos cm_file =
  let no_ext = Filename.remove_extension cm_file in
  assert(no_ext = Filename.remove_extension file_infos.cm_file);
  match Filename.extension cm_file, file_infos with
  | ".cmt", {annots; cmti_uid_to_decl; _} ->
      let* res, cmt_infos = init_from_cm_file cm_file in
      let+ location_dependencies =
        match file_infos.location_dependencies with
        | [] -> Location_dependencies.init ~comp_unit_to_path cmt_infos cmti_uid_to_decl
        | loc_dep -> (* They have already been computed *)
            Result.ok loc_dep
      in
      let annots =
        match merge_annots annots res.annots with
        | Error _ ->
            (* TODO: better error handling *)
            assert false
        | Ok annots -> annots
      in
      {res with annots; cmti_uid_to_decl; location_dependencies}
  | ".cmti", {cmti_uid_to_decl = (Some _ as cutd); annots; location_dependencies; _} ->
      let+ res, _ = init_from_cm_file cm_file in
      {res with cmti_uid_to_decl = cutd; annots; location_dependencies}
  | _ ->
      (* invalid extension or the corresponding info is None *)
      init ~comp_unit_to_path cm_file

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
  | Some sourcepath -> Utils.Filepath.unit sourcepath
  | None -> "!!UNKNOWN_SOURCEUNIT_FOR<" ^ t.cm_file ^ ">!!"

let get_modname t = t.modname
