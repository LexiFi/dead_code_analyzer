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

  let init_from_cmt cmt_file =
    if not (Sys.file_exists cmt_file) then Result.error (cmt_file ^ ": file not found")
    else
      try
        let cmt_infos = Cmt_format.read_cmt cmt_file in
        let builddir = cmt_infos.cmt_builddir in
        let sourcepath =
          match cmt_infos.cmt_sourcefile with
          | Some sourcefile -> Filename.concat builddir sourcefile
          | None ->
            Printf.sprintf "!!UNKNOWN_SOURCEPATH_IN<%s>_FOR_<%s>!!"
              builddir
              cmt_file
        in
        let modname = cmt_infos.cmt_modname in
        Result.ok {
          empty with cmti_file = cmt_file;
                     builddir;
                     sourcepath;
                     modname;
                     cmt_infos = Some cmt_infos;
        }
      with _ -> Result.error (cmt_file ^ ": cannot read cmt file")

  let init_from_cmi ?with_cmt cmi_file =
    if not (Sys.file_exists cmi_file) then Result.error (cmi_file ^ ": file not found")
    else
      try
        let cmi_infos = Cmi_format.read_cmi cmi_file in
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
          match cmi_infos.cmi_sign with
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
        Result.ok {
          empty with cmti_file = cmi_file;
                     builddir;
                     sourcepath;
                     modname;
                     cmi_infos = Some cmi_infos;
        }
      with _ -> Result.error (cmi_file ^ ": cannot read cmi file")

  let init cmti_file =
    let no_ext = Filename.remove_extension cmti_file in
    let ext = Filename.extension cmti_file in
    let with_cmt = init_from_cmt (no_ext ^ ".cmt") in
    let with_cmi =
      let with_cmt = Result.to_option with_cmt in
      init_from_cmi ?with_cmt (no_ext ^ ".cmi")
    in
    match with_cmt, with_cmi with
    | Error _, Error _ -> Result.error (cmti_file ^ ": cannot read cmi nor cmt info")
    | ok, Error _
    | Error _, ok -> ok
    | Ok with_cmt, Ok {cmi_infos; _} when ext = ".cmt" -> Result.ok {with_cmt with cmti_file; cmi_infos}
    | Ok {cmt_infos; _}, Ok with_cmi when ext = ".cmi" -> Result.ok {with_cmi with cmti_file; cmt_infos}
    | _, _ -> Result.error (cmti_file ^ ": not a cmi or cmt file")

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

let current = ref empty

let get_current () = !current

let update state = current := state
