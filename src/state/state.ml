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

let report_section_is_enabled ~elt_kind state =
  match elt_kind with
  | `Ctor_field -> Config.must_report_section state.config.sections.types
  | `Method _
  | `Object -> Config.must_report_section state.config.sections.methods
  | `Value -> Config.must_report_section state.config.sections.exported_values

let add_exported_declaration ~elt_kind ~elt_loc ~elt_path state =
  if not (report_section_is_enabled ~elt_kind state) then state
  else
    let builddir = File_infos.get_builddir state.file_infos in
    match elt_kind with
    | `Method meth_name ->
        let methods =
          Methods.add_exported_declaration
            ~builddir ~obj_loc:elt_loc ~meth_name ~meth_path:elt_path
            state.methods
        in
        { state with methods }
    | `Ctor_field ->
        let ctors_fields =
          Ctors_fields.add_exported_declaration
            ~builddir ~cf_loc:elt_loc ~cf_path:elt_path
            state.ctors_fields
        in
        { state with ctors_fields }
    | `Value ->
        let values =
          Values.add_exported_declaration
            ~builddir ~val_loc:elt_loc ~val_path:elt_path
            state.values
        in
        { state with values }

let remove_exported_declaration ~elt_kind ~elt_loc state =
  let builddir = File_infos.get_builddir state.file_infos in
  match elt_kind with
  | `Method meth_name ->
      let methods =
        Methods.remove_exported_declaration
          ~builddir ~obj_loc:elt_loc ~meth_name
          state.methods
      in
      { state with methods }
  | `Object ->
      let methods =
        Methods.remove_exported_declarations
          ~builddir ~obj_loc:elt_loc
          state.methods
      in
      { state with methods }
  | `Ctor_field ->
      let ctors_fields =
        Ctors_fields.remove_exported_declaration
          ~builddir ~cf_loc:elt_loc
          state.ctors_fields
      in
      { state with ctors_fields }
  | `Value ->
      let values =
        Values.remove_exported_declaration
          ~builddir ~val_loc:elt_loc
          state.values
      in
      { state with values }

let is_exported_declaration ~elt_kind ~elt_loc state =
  match elt_kind with
  | `Object ->
      Methods.is_exported_declaration ~obj_loc:elt_loc state.methods
  | `Ctor_field ->
      Ctors_fields.is_exported_declaration ~cf_loc:elt_loc state.ctors_fields
  | `Value ->
      Values.is_exported_declaration ~val_loc:elt_loc state.values

let get_exported_declaration_path ~elt_kind ~builddir ~elt_loc state =
  match elt_kind with
  | `Method meth_name ->
      Methods.get_meth_path ~builddir ~obj_loc:elt_loc ~meth_name state.methods
  | `Ctor_field ->
      Ctors_fields.get_cf_path ~builddir ~cf_loc:elt_loc state.ctors_fields
  | `Value ->
      Values.get_val_path ~builddir ~val_loc:elt_loc state.values


let should_track_use ~elt_kind ~elt_loc ~use_loc state =
  (* Uses are discarded if they should not be tracked for the given element.
     This is the case when the corrresponding report section is disabled
     or if the element is a value, the use is internal, and tracking
     internal uses is disabled.
  *)
  let should_track_value_use elt_loc use_loc state =
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
    state.config.internal || is_exported ()
  in
  match elt_kind with
  | `Ctor_field | `Method _ ->
      report_section_is_enabled ~elt_kind state
  | `Value ->
      report_section_is_enabled ~elt_kind state
      && should_track_value_use elt_loc use_loc state

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

let remove_uses ~elt_kind ~elt_loc state =
  match elt_kind with
  | `Method meth_name ->
      let methods =
        Methods.remove_uses ~obj_loc:elt_loc ~meth_name state.methods
      in
      { state with methods }
  | `Object ->
      let methods =
        Methods.remove_uses ~obj_loc:elt_loc state.methods
      in
      { state with methods }
  | `Ctor_field ->
      let ctors_fields =
        Ctors_fields.remove_uses ~cf_loc:elt_loc state.ctors_fields
      in
      { state with ctors_fields }
  | `Value ->
      let values =
        Values.remove_uses ~val_loc:elt_loc state.values
      in
      { state with values }

let get_uses ~elt_kind ~elt_loc state =
  match elt_kind with
  | `Method meth_name ->
      Methods.get_uses ~obj_loc:elt_loc ~meth_name state.methods
  | `Ctor_field ->
      Ctors_fields.get_uses ~cf_loc:elt_loc state.ctors_fields
  | `Value ->
      Values.get_uses ~val_loc:elt_loc state.values

let add_self_use ~elt_kind ~elt_loc ~use_loc state =
  if not (should_track_use ~elt_kind ~elt_loc ~use_loc state) then state
  else
    match elt_kind with
    | `Method meth_name ->
        let methods =
          Methods.add_self_use ~obj_loc:elt_loc ~meth_name ~use_loc state.methods
        in
        { state with methods }

let add_alias ~elt_kind ~orig_loc ~alias_loc state =
  if not (report_section_is_enabled ~elt_kind state) then state
  else
    match elt_kind with
    | `Object ->
        let methods =
          Methods.add_alias ~orig_loc ~alias_loc state.methods
        in
        { state with methods }
    | `Ctor_field ->
        let ctors_fields =
          Ctors_fields.add_alias ~orig_loc ~alias_loc state.ctors_fields
        in
        { state with ctors_fields }
    | `Value ->
        let values =
          Values.add_alias ~orig_loc ~alias_loc state.values
        in
        { state with values }

let add_alias ~elt_kind ~orig_loc ~alias_loc state =
  let state =
    (* An immediate object is a value with methods. Thus, a value alias
       may also be an immediate object alias.
    *)
    match elt_kind with
    | `Value -> add_alias ~elt_kind:`Object ~alias_loc ~orig_loc state
    | `Object | `Ctor_field -> state
  in
  add_alias ~elt_kind ~orig_loc ~alias_loc state

type element =
  [ `Ctor_field | `Method of string | `Value ] (* elt kind *)
  * Lexing.position (* elt loc *)
  * string (* elt builddir *)

let get_unused ~elt_kind ?(max_uses = 0) state =
  let with_elt_kind ~add_elt_kind tbl =
    let res = Hashtbl.create (max_uses + 1) in
    Hashtbl.iter
      (fun nb_uses elts ->
        List.map add_elt_kind elts
        |> Hashtbl.add res nb_uses
      )
      tbl;
    res
  in
  match elt_kind with
  | `Object ->
      let add_elt_kind (obj_loc, meth_name, builddir) =
        (`Method meth_name, obj_loc, builddir)
      in
      Methods.get_unused ~max_uses state.methods
      |> with_elt_kind ~add_elt_kind
  | `Ctor_field ->
      let add_elt_kind (cf_loc, builddir) =
        (`Ctor_field, cf_loc, builddir)
      in
      Ctors_fields.get_unused ~max_uses state.ctors_fields
      |> with_elt_kind ~add_elt_kind
  | `Value ->
      let add_elt_kind (val_loc, builddir) =
        (`Value, val_loc, builddir)
      in
      Values.get_unused ~max_uses state.values
      |> with_elt_kind ~add_elt_kind

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
