module Locdep = Location_dependencies

type cm_infos =
  | Cmti of {
        sign : Typedtree.signature;
        cmti_uid_to_decl : Location_dependencies.uid_to_decl;
      }
  | Cmt of {
        strc : Typedtree.structure;
        sign : Typedtree.signature option;
        location_dependencies : Location_dependencies.t;
      }
  | Neither

type t = {
  builddir : string;
  cm_file : string;
  cm_infos : cm_infos;
  modname : string;
  sourcepath : string option;
}

let empty = {
  builddir = "!!UNKNOWN_BUILDDIR!!";
  cm_file = "";
  cm_infos = Neither;
  modname = "!!UNKNOWN_MODNAME!!";
  sourcepath = None;
}

(** [init_from_all_cm_infos ~cm_file cmt_infos] creates a [t] with:
    - information from [cmt_infos] : [builddir], [modname], [sourcepath];
    - [cm_file];
    - [cm_infos] is built with [cmt_infos.cmt_annots] and
      [cmt_infos.cmt_uid_to_decl]
*)
let init_from_all_cm_infos ~cm_file cmt_infos =
  let builddir = cmt_infos.Cmt_format.cmt_builddir in
  let sourcepath =
    Option.map Utils.Filepath.remove_pp cmt_infos.cmt_sourcefile
    |> Option.map (Filename.concat builddir)
  in
  let modname = cmt_infos.cmt_modname in
  let cm_infos =
    match cmt_infos.cmt_annots with
    | Interface sign ->
        let cmti_uid_to_decl = cmt_infos.cmt_uid_to_decl in
        Cmti {sign; cmti_uid_to_decl}
    | Implementation strc ->
        let location_dependencies = Location_dependencies.empty in
        Cmt {strc; sign = None; location_dependencies}
    | _ -> Neither
  in
  Utils.Envaux.set_loadpaths cmt_infos.cmt_loadpath;
  {builddir; cm_file; cm_infos; modname; sourcepath}

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
    | Ok cmt_infos ->
        let file_infos =
          init_from_all_cm_infos ~cm_file cmt_infos
        in
        Result.ok (file_infos, cmt_infos)

let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x

let init_from_cmti_file cmti_file =
  let* file_infos, _ = init_from_cm_file cmti_file in
  match file_infos.cm_infos with
  | Cmti _ -> Result.ok file_infos
  | _ -> Result.error (cmti_file ^ ": does not contain an interface")

let init_from_cmt_file ~comp_unit_to_path ?(cmi_infos = Neither) cmt_file =
  let* file_infos, cmt_infos = init_from_cm_file cmt_file in
  match file_infos.cm_infos with
  | Cmt cmt ->
      let sign, cmti_uid_to_decl =
        match cmi_infos with
        | Cmti {sign; cmti_uid_to_decl} -> Some sign, Some cmti_uid_to_decl
        | _ -> None, None
      in
      let+ location_dependencies =
        Locdep.init ~comp_unit_to_path cmt_infos cmti_uid_to_decl
      in
      let cm_infos = Cmt {cmt with sign; location_dependencies} in
      {file_infos with cm_infos}
  | _ -> Result.error (cmt_file ^ ": does not contain an implementation")

let init ~comp_unit_to_path cm_file =
  match Filename.extension cm_file with
  | ".cmt" -> init_from_cmt_file ~comp_unit_to_path cm_file
  | ".cmti" -> init_from_cmti_file cm_file
  | _ -> Result.error (cm_file ^ ": not a .cmti or .cmt file")

let change_file ~comp_unit_to_path file_infos cm_file =
  let no_ext = Filename.remove_extension cm_file in
  assert(no_ext = Filename.remove_extension file_infos.cm_file);
  match Filename.extension cm_file, file_infos with
  | ".cmt", {cm_infos = Cmti _ as cmi_infos; _} ->
        init_from_cmt_file ~comp_unit_to_path ~cmi_infos cm_file
  | ".cmt",  _ ->
      (* The only other possible file read before a .cmt is the
         corresponding .cmti *)
      Result.error (cm_file ^ ": must be read after its corresponding .cmti")
  | ".cmti", _ ->
      (* .cmti files are always read before the corresponding .cmt *)
      Result.error (cm_file ^ ": must be read before its corresponding .cmt")
  | _ ->
      (* invalid extension *)
      Result.error (cm_file ^ ": not a .cmti or .cmt file")

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
