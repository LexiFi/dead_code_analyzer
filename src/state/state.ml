module File_infos = File_infos

module Values = Values
module Methods = Methods
module Ctors_fields = Ctors_fields

type t =
  { config : Config.t
  ; comp_unit_to_path : (string, string) Hashtbl.t
  ; file_infos : File_infos.t
  ; values : Values.t
  ; methods : Methods.t
  ; ctors_fields : Ctors_fields.t
  }

(* config-related manipulations *)

let init config =
  Cmt.set_cache_size config.Config.cmt_cache_size;
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
  ; values = Values.create ()
  ; methods = Methods.create ()
  ; ctors_fields = Ctors_fields.create ()
  }

let update_config config state =
  Cmt.set_cache_size config.Config.cmt_cache_size;
  {state with config}

(* file-related manipulations *)

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

(* code-element related manipulations *)

let should_track_use ~elt_kind ~elt_loc ~use_loc state =
  (* Uses are discarded if they should not be tracked for the given element.
     This is the case when the corrresponding report section is disabled
     or if the element is a value, the use is internal, and tracking
     internal uses is disabled.
  *)
  match elt_kind with
  | `Ctor_field -> Config.must_report_section state.config.sections.types
  | `Method _ -> Config.must_report_section state.config.sections.methods
  | `Value ->
      let is_external () =
        let elt_fname = elt_loc.Lexing.pos_fname in
        String.ends_with ~suffix:"i" elt_fname (* elt_loc is in a .mli *)
        || ( (* compare elt and use compilation units *)
          let elt_unit = Utils.Filepath.unit elt_fname in
          let use_fname = use_loc.Lexing.pos_fname in
          let use_unit = Utils.Filepath.unit use_fname in
          not (String.equal use_unit elt_unit))
      in
      let is_exported () =
        match state.file_infos.cm_infos with
        | _ when is_external () -> true
        | Cmt {sign = None; _} ->
            (* is_defined in current .ml but there is no .mli *)
            true
        | _ -> false
      in
      Config.must_report_section state.config.sections.exported_values
      && (state.config.internal || is_exported ())

let add_use ~elt_kind ~elt_loc ~use_loc state =
  if not (should_track_use ~elt_kind ~elt_loc ~use_loc state) then state
  else
    match elt_kind with
    | `Method meth_name ->
        let methods =
          Methods.add_use ~obj_loc:elt_loc ~meth_name ~use_loc state.methods
        in
        { state with methods }
    | `Ctor_field ->
        let ctors_fields =
          Ctors_fields.add_use ~cf_loc:elt_loc ~use_loc state.ctors_fields
        in
        { state with ctors_fields }
    | `Value ->
        let values =
          Values.add_use ~val_loc:elt_loc ~use_loc state.values
        in
        { state with values }

let add_self_use ~elt_kind ~elt_loc ~use_loc state =
  if not (should_track_use ~elt_kind ~elt_loc ~use_loc state) then state
  else
    match elt_kind with
    | `Method meth_name ->
        let methods =
          Methods.add_self_use ~obj_loc:elt_loc ~meth_name ~use_loc state.methods
        in
        { state with methods }

(** Analysis' state *)
let current = ref
    { config = Config.default_config
    ; comp_unit_to_path = Hashtbl.create 0
    ; file_infos = File_infos.empty
    ; values = Values.create ()
    ; methods = Methods.create ()
    ; ctors_fields = Ctors_fields.create ()
    }

let get_current () = !current

let update state = current := state
