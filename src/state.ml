module File_infos = struct
  type t = {
    cmti_file : string;
    sourcepath : string;
    builddir : string;
    modname : string;
    cmi_infos : Cmi_format.cmi_infos option;
    cmt_infos : Cmt_format.cmt_infos option;
  }

  let empty = {
    cmti_file = "";
    sourcepath = "!!UNKNOWN_SOURCEPATH!!";
    builddir = "!!UNKNOWN_BUILDDIR!!";
    modname = "!!UNKNOWN_MODNAME!!";
    cmi_infos = None;
    cmt_infos = None;
  }

  let init_from_cmt_infos cmt_infos cmt_file =
    let builddir = cmt_infos.Cmt_format.cmt_builddir in
    let sourcepath =
      match cmt_infos.cmt_sourcefile with
      | Some sourcefile -> Filename.concat builddir sourcefile
      | None ->
        Printf.sprintf "!!UNKNOWN_SOURCEPATH_IN<%s>_FOR_<%s>!!"
          builddir
          cmt_file
    in
    let modname = cmt_infos.cmt_modname in
    {empty with cmti_file = cmt_file;
                builddir;
                sourcepath;
                modname;
                cmt_infos = Some cmt_infos;
    }

  let init_from_cmt cmt_file =
    if not (Sys.file_exists cmt_file) then Result.error (cmt_file ^ ": file not found")
    else
      try
        let cmt_infos = Cmt_format.read_cmt cmt_file in
        init_from_cmt_infos cmt_infos cmt_file
        |> Result.ok
      with _ -> Result.error (cmt_file ^ ": cannot read cmt file")

  let init_from_cmi_infos ?with_cmt cmi_infos cmi_file =
    let builddir =
      match with_cmt with
      | None -> "!!UNKNOWN_BUILDDIR_FOR<" ^ cmi_file ^ ">!!"
      | Some {builddir; _} -> builddir
    in
    let sourcepath =
      let unknown_sourcepath =
        match with_cmt with
        | None -> "!!UNKNOWN_SOURCEPATH_FOR<" ^ cmi_file ^ ">!!"
        | Some {sourcepath; _} -> sourcepath
      in
      match cmi_infos.Cmi_format.cmi_sign with
      | [] -> unknown_sourcepath
      | sig_item::_ -> match sig_item with
        (* assume that the first item's location points to the interface
           file (.mli) if it is not a ghost location *)
        | Sig_value (_, {Types.val_loc = loc; _}, _)
        | Sig_type (_, {Types.type_loc = loc; _}, _, _)
        | Sig_typext (_, {Types.ext_loc = loc; _}, _, _)
        | Sig_module (_, _, {Types.md_loc = loc; _}, _, _)
        | Sig_modtype (_,  {Types.mtd_loc = loc; _}, _)
        | Sig_class (_,  {Types.cty_loc = loc; _}, _, _)
        | Sig_class_type (_,  {Types.clty_loc = loc; _}, _, _) ->
          if loc.Location.loc_ghost then unknown_sourcepath
          else Filename.concat builddir loc.Location.loc_start.pos_fname
    in
    let modname = cmi_infos.cmi_name in
    {empty with cmti_file = cmi_file;
                builddir;
                sourcepath;
                modname;
                cmi_infos = Some cmi_infos;
    }

  let init_from_cmi ?with_cmt cmi_file =
    if not (Sys.file_exists cmi_file) then Result.error (cmi_file ^ ": file not found")
    else
      try
        let cmi_infos = Cmi_format.read_cmi cmi_file in
        init_from_cmi_infos ?with_cmt cmi_infos cmi_file
        |> Result.ok
      with _ -> Result.error (cmi_file ^ ": cannot read cmi file")

  let init cmti_file =
    let no_ext = Filename.remove_extension cmti_file in
    match Filename.extension cmti_file with
    | ".cmi" ->
      let with_cmt = init_from_cmt (no_ext ^ ".cmt") |> Result.to_option in
      init_from_cmi ?with_cmt cmti_file
    | ".cmt" ->
      let with_cmi_infos with_cmt =
        match init_from_cmi ~with_cmt (no_ext ^ ".cmi") with
        | Error _ -> with_cmt
        | Ok {cmi_infos; _} -> {with_cmt with cmi_infos}
      in
      init_from_cmt cmti_file |> Result.map with_cmi_infos
    | _ -> Result.error (cmti_file ^ ": not a .cmi or .cmt file")

  let change_file file_infos cmti_file =
    let no_ext = Filename.remove_extension cmti_file in
    assert(no_ext = Filename.remove_extension file_infos.cmti_file);
    match Filename.extension cmti_file, file_infos with
    | ".cmi", {cmi_infos=Some cmi_infos; _} ->
      let res = init_from_cmi_infos ~with_cmt:file_infos cmi_infos cmti_file in
      Result.ok res
    | ".cmt", {cmt_infos = Some cmt_infos; cmi_infos; _} ->
      let res = init_from_cmt_infos cmt_infos cmti_file in
      Result.ok {res with cmi_infos}
    | ".cmi", _
    | ".cmt", _ -> (* corresponding info is None *)
      init cmti_file
    | _ -> Result.error (cmti_file ^ ": not a .cmi or .cmt file")

  let get_builddir t = t.builddir

  let get_sourcepath t = t.sourcepath

  let get_sourceunit t =
    match t.cmt_infos with
    | Some {cmt_sourcefile = Some _; _} ->
      get_sourcepath t |> Filename.basename |> Filename.remove_extension
    | _ -> "!!UNKNOWN_SOURCEUNIT_FOR<" ^ t.cmti_file ^ ">!!"

  let get_modname t = t.modname

end

type t = {
  file_infos : File_infos.t;
}

let empty = {file_infos = File_infos.empty}

let init cmti_file =
  let file_infos = File_infos.init cmti_file in
  Result.map (fun file_infos -> {file_infos}) file_infos

let change_file state cmti_file =
  let file_infos = state.file_infos in
  let equal_no_ext filename1 filename2 =
    let no_ext1 = Filename.remove_extension filename1 in
    let no_ext2 = Filename.remove_extension filename2 in
    no_ext1 = no_ext2
  in
  if file_infos.cmti_file = cmti_file then
    Result.ok state
  else if equal_no_ext file_infos.cmti_file cmti_file then
    let file_infos = File_infos.change_file file_infos cmti_file in
    Result.map (fun file_infos -> {file_infos}) file_infos
  else
    init cmti_file


let current = ref empty

let get_current () = !current

let update state = current := state
