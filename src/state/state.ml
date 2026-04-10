module File_infos = File_infos

type t =
  { config : Config.t
  ; comp_unit_to_path : (string, string) Hashtbl.t
  ; file_infos : File_infos.t
  }

let init config =
  let comp_unit_to_path =
    let tbl = Hashtbl.create 32 in
    Utils.StringSet.iter (fun filepath ->
        let comp_unit = Utils.Filepath.unit filepath in
        Hashtbl.add tbl comp_unit filepath)
        config.Config.paths_to_analyze;
    tbl
  in
  { config
  ; comp_unit_to_path
  ; file_infos = File_infos.empty
  }

let update_config config state =
  {state with config}


let change_file state cm_file =
  let file_infos = state.file_infos in
  let comp_unit_to_path = state.comp_unit_to_path in
  let equal_no_ext filename1 filename2 =
    let no_ext1 = Filename.remove_extension filename1 in
    let no_ext2 = Filename.remove_extension filename2 in
    String.equal no_ext1 no_ext2
  in
  if String.equal file_infos.cm_file cm_file then
    Result.ok state
  else if equal_no_ext file_infos.cm_file cm_file then
    let file_infos = File_infos.change_file ~comp_unit_to_path file_infos cm_file in
    Result.map (fun file_infos -> {state with file_infos}) file_infos
  else
    let file_infos = File_infos.init ~comp_unit_to_path cm_file in
    Result.map (fun file_infos -> {state with file_infos}) file_infos

(** Analysis' state *)
let current = ref
    { config = Config.default_config
    ; comp_unit_to_path = Hashtbl.create 0
    ; file_infos = File_infos.empty
    }

let get_current () = !current

let update state = current := state
