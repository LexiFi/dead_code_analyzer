module File_infos = File_infos

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

(** Analysis' state *)
let current = ref empty

let get_current () = !current

let update state = current := state
