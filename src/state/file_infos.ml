type t = {
  cmti_file : string;
  sourcepath : string option;
  builddir : string option;
  modname : string;
  cmi_infos : Cmi_format.cmi_infos option;
  cmt_infos : Cmt_format.cmt_infos option;
}

let empty = {
  cmti_file = "";
  sourcepath = None;
  builddir = None;
  modname = "!!UNKNOWN_MODNAME!!";
  cmi_infos = None;
  cmt_infos = None;
}

(** [init_from_cmt_infos cmt_infos cmt_file] creates a [t] with:
    - information from [cmt_infos];
    - [cmti_file = cmt_file];
    - [cmt_infos = Some cmt_infos]. *)
let init_from_cmt_infos cmt_infos cmt_file =
  let builddir = cmt_infos.Cmt_format.cmt_builddir in
  let sourcepath =
    Option.map (Filename.concat builddir) cmt_infos.cmt_sourcefile
  in
  let modname = cmt_infos.cmt_modname in
  {empty with cmti_file = cmt_file;
              builddir = Some builddir;
              sourcepath;
              modname;
              cmt_infos = Some cmt_infos;
  }

(** [init_from_cmt cmt_file] returns an [Ok t] with [t] filled using
    the [cmt_file] (see [init_from_cmt_infos]).
    In case the file does not exist or it cannot be read (see
    [Cmt_format.read_cmt]) then it returns an [Err msg] with msg a string
    describing the issue. *)
let init_from_cmt cmt_file =
  if not (Sys.file_exists cmt_file) then Result.error (cmt_file ^ ": file not found")
  else
    try
      let cmt_infos = Cmt_format.read_cmt cmt_file in
      init_from_cmt_infos cmt_infos cmt_file
      |> Result.ok
    with _ -> Result.error (cmt_file ^ ": cannot read cmt file")


let sourcefname_of_cmi_infos cmi_unit cmi_infos =
  (* Use lowercased units because dune wrapped lib's module units follow the
     pattern : `<lib>__<Captilized_module>` while the original module unit may
     not be capitalized.
  *)
  let cmi_unit = String.lowercase_ascii cmi_unit in
  let candidate_of_fname fname =
    let src_unit = Utils.unit fname |> String.lowercase_ascii in
    if String.equal src_unit cmi_unit then
      `Identical fname
    else if String.ends_with ~suffix:src_unit cmi_unit then
      `Suffix fname
    else `Different
  in
  let fname_of_candidate = function
    | `Different -> None
    | `Identical fname
    | `Suffix fname -> Some fname
  in
  let get_item_loc (sig_item : Types.signature_item) =
    match sig_item with
    | Sig_value (_, {val_loc = loc; _}, _)
    | Sig_type (_, {type_loc = loc; _}, _, _)
    | Sig_typext (_, {ext_loc = loc; _}, _, _)
    | Sig_module (_, _, {md_loc = loc; _}, _, _)
    | Sig_modtype (_,  {mtd_loc = loc; _}, _)
    | Sig_class (_,  {cty_loc = loc; _}, _, _)
    | Sig_class_type (_,  {clty_loc = loc; _}, _, _) ->
      loc
  in
  let rec find_sourcename candidate = function
  | [] -> fname_of_candidate candidate
  | sig_item::items ->
    let loc = get_item_loc sig_item in
    if loc.Location.loc_ghost then find_sourcename candidate items
    else
      let fname = loc.Location.loc_start.pos_fname in
      match candidate, candidate_of_fname fname with
      | (`Identical _ as candidate), _
      | _, (`Identical _ as candidate) ->
        (* best candidate found *)
        fname_of_candidate candidate
      | `Different, candidate
      | candidate, `Different
      | _, candidate ->
        find_sourcename candidate items
  in
  find_sourcename `Different cmi_infos.Cmi_format.cmi_sign

(** [init_from_cmi_infos ?with_cmt cmi_infos cmi_file] creates a [t] with:
    - information from [cmt_infos];
    - [cmti_file = cmt_file];
    - [cmi_infos = Some cmi_infos].
    Because the [cmi_infos] is not as complete as [cmt_infos] (e.g. it does not
    specify the [builddir]), an existing [t] filled using the correpsonding
    [cmt_infos] can be passed as argument. In this case, information unavailable
    in the [cmi_infos] is read from [with_cmt]. Otherwise, default values are
    set for [builddir] and eventually [sourcepath]. *)
let init_from_cmi_infos ?with_cmt cmi_infos cmi_file =
  let builddir = Option.bind with_cmt (fun {builddir; _} -> builddir) in
  let sourcepath =
    let sourcepath =
      (* Try to find a sourcepath in the cmi_infos *)
      let cmi_unit = Utils.unit cmi_file in
      let sourcefname = sourcefname_of_cmi_infos cmi_unit cmi_infos in
      match sourcefname, builddir with
      | Some fname, Some builddir -> Some (Filename.concat builddir fname)
      | _, _ -> sourcefname
    in
    match sourcepath with
    | Some _ -> sourcepath
    | None ->
      (* There is no satisfying sourcepath in the cmi_infos.
         Try to retrieve the sourecpath using with_cmt.
      *)
      let sourcepath_of_cmt cmt_file sourcepath =
        (* When producing .cmt files for .ml files, the compiler also produces
           .cmti files for .mli files. Hence, if a .cmti exists, we assume the
           .mli does.
        *)
        if Sys.file_exists (cmt_file ^ "i") then sourcepath ^ "i"
        else sourcepath
      in
      Option.bind with_cmt
        (fun {sourcepath; cmti_file; _} ->
          Option.map (sourcepath_of_cmt cmti_file) sourcepath
        )

  in
  let modname = cmi_infos.cmi_name in
  {empty with cmti_file = cmi_file;
              builddir;
              sourcepath;
              modname;
              cmi_infos = Some cmi_infos;
  }

(** [init_from_cmi cmi_file] returns an [Ok t] with [t] filled using
    the [cmi_file] (see [init_from_cmi_infos]).
    In case the file does not exist or it cannot be read (see
    [Cmi_format.read_cmi]) then it returns an [Err msg] with msg a string
    describing the issue. *)
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
  | _ -> (* corresponding info is None *)
    init cmti_file

let has_builddir file_infos = Option.is_some file_infos.builddir

let has_sourcepath file_infos = Option.is_some file_infos.sourcepath

let get_builddir t =
  match t.builddir with
  | Some builddir -> builddir
  | None -> "!!UNKNOWN_BUILDDIR_FOR<" ^ t.cmti_file ^ ">!!"

let get_sourcepath t =
  match t.sourcepath with
  | Some sourcepath -> sourcepath
  | None -> match t.builddir with
    | Some builddir ->
      Printf.sprintf "!!UNKNOWN_SOURCEPATH_IN<%s>_FOR_<%s>!!"
        builddir
        t.cmti_file
    | None -> "!!UNKNOWN_SOURCEPATH_FOR<" ^ t.cmti_file ^ ">!!"

let get_sourceunit t =
  match t.sourcepath with
  | Some sourcepath -> Utils.unit sourcepath
  | None -> "!!UNKNOWN_SOURCEUNIT_FOR<" ^ t.cmti_file ^ ">!!"

let get_modname t = t.modname
