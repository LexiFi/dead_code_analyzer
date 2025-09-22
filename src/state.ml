module File_infos = struct
  type t = {
    cmti_file : string;
    cmi_infos : Cmi_format.cmi_infos option;
    cmt_infos : Cmt_format.cmt_infos option;
  }

  let empty = {
    cmti_file = "";
    cmi_infos = None;
    cmt_infos = None;
  }

  let init_from_cmt cmt_file =
    if not (Sys.file_exists cmt_file) then Result.error (cmt_file ^ ": file not found")
    else
      try
        let cmt_infos = Cmt_format.read_cmt cmt_file in
        Result.ok {
          empty with cmti_file = cmt_file;
                     cmt_infos = Some cmt_infos
        }
      with _ -> Result.error (cmt_file ^ ": cannot read cmt file")

  let init_from_cmi cmi_file =
    if not (Sys.file_exists cmi_file) then Result.error (cmi_file ^ ": file not found")
    else
      try
        let cmi_infos = Cmi_format.read_cmi cmi_file in
        Result.ok {
          empty with cmti_file = cmi_file;
                     cmi_infos = Some cmi_infos
        }
      with _ -> Result.error (cmi_file ^ ": cannot read cmi file")

  let init cmti_file =
    let no_ext = Filename.remove_extension cmti_file in
    let with_cmt = init_from_cmt (no_ext ^ ".cmt") in
    let with_cmi = init_from_cmi (no_ext ^ ".cmi") in
    match with_cmt, with_cmi with
    | Error _, Error _ -> Result.error (cmti_file ^ ": cannot read cmi nor cmt info")
    | ok, Error _
    | Error _, ok -> ok
    | Ok with_cmt, Ok {cmi_infos; _} -> Result.ok {with_cmt with cmti_file; cmi_infos}

  let get_builddir t =
    match t.cmt_infos with
    | None -> "!!UNKNOWN_BUILDDIR_FOR<" ^ t.cmti_file ^ ">!!"
    | Some {cmt_builddir; _} -> cmt_builddir

  let get_sourcepath t =
    match t.cmt_infos with
    | None -> "!!UNKNOWN_SOURCEPATH_FOR<" ^ t.cmti_file ^ ">!!"
    | Some {cmt_sourcefile; _} ->
      let builddir = get_builddir t in
      match cmt_sourcefile with
      | None -> "!!UNKNOWN_SOURCEPATH_IN<" ^ builddir ^ ">_FOR_<" ^ t.cmti_file ^ ">!!"
      | Some sourcefile -> Filename.concat builddir sourcefile

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
