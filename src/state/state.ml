module File_infos = File_infos

type t =
  { config : Config.t
  ; file_infos : File_infos.t
  ; signature : Signature.t
  }

let init config =
  { config
  ; file_infos = File_infos.empty
  ; signature = Signature.empty ()
  }

let update_config config state =
  {state with config}

let change_file state cm_file =
  let file_infos = state.file_infos in
  let equal_no_ext filename1 filename2 =
    let no_ext1 = Filename.remove_extension filename1 in
    let no_ext2 = Filename.remove_extension filename2 in
    String.equal no_ext1 no_ext2
  in
  if String.equal file_infos.cm_file cm_file then
    Result.ok state
  else if equal_no_ext file_infos.cm_file cm_file then
    let file_infos = File_infos.change_file file_infos cm_file in
    Result.map (fun file_infos -> {state with file_infos}) file_infos
  else
    let ( let* ) x f = Result.bind x f in
    let* file_infos = File_infos.init cm_file in
    let* signature = Signature.init file_infos in
    Result.ok {state with file_infos; signature}

(** Analysis' state *)
let current = ref
    { config = Config.default_config
    ; file_infos = File_infos.empty
    ; signature = Signature.empty ()
    }

let get_current () = !current

let update state = current := state
