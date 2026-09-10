module Filepath = struct

  type t = string

  let remove_pp filepath =
    let ext = Filename.extension filepath in
    let no_ext = Filename.remove_extension filepath in
    match Filename.extension no_ext with
    | ".pp" -> Filename.remove_extension no_ext ^ ext
    | _ -> filepath

  let unit filepath =
    #if OCAML_VERSION >= (5, 3, 0)
    Unit_info.lax_modname_from_source filepath
    #else
    (* reproduce https://github.com/ocaml/ocaml/blob/5.3/parsing/unit_info.ml#L60 *)
    let remove_all_ext basename =
      match String.index basename '.' with
        | dot_pos -> String.sub basename 0 dot_pos
        | exception Not_found -> basename
    in
    filepath |> Filename.basename |> remove_all_ext |> String.capitalize_ascii
    #endif

  type kind =
    | Cmti
    | Cmt_without_mli
    | Cmt_with_mli
    | Dir
    | Ignore

  (* Checks the nature of the file *)
  let kind ~exclude filepath =
    if exclude filepath then Ignore
    else if not (Sys.file_exists filepath) then (
      prerr_endline ("Warning: '" ^ filepath ^ "' not found");
      Ignore
    )
    else if Sys.is_directory filepath then Dir
    else if Filename.check_suffix filepath ".cmti" then Cmti
    else if Filename.check_suffix filepath ".cmt" then
      let cmti = Filename.remove_extension filepath ^ ".cmti" in
      if Sys.file_exists cmti then Cmt_with_mli
      else Cmt_without_mli
    else Ignore
end

let rec signature_of_modtype ?(select_param = false) modtype =
  let open Types in
  match modtype with
  | Mty_signature sg -> sg
  | Mty_functor (_, t) when not select_param -> signature_of_modtype t
  | Mty_functor (Named (_, t), _) -> signature_of_modtype t
  | _ -> []

let rec typedtree_signature_of_modtype ?(select_param = false) modtype =
  let open Typedtree in
  match modtype.mty_desc with
  | Tmty_signature sg -> Some sg
  | Tmty_functor (_, t) when not select_param -> typedtree_signature_of_modtype t
  | Tmty_functor (Named (_, _, t), _) -> typedtree_signature_of_modtype t
  | _ -> None

module StringSet = Set.Make(String)

module Envaux = struct
  (* Lazy set up of loadpaths for load_env.
     This is used to ensure the setup is applied only once and only if
     necessary
  *)
  let setup = ref (Lazy.from_val ())
  let force_setup () = Lazy.force !setup

  type paths =
    #if OCAML_VERSION >= (5, 2, 0)
    Load_path.paths
    #else
    string list
    #endif

  let init_load_path paths =
    #if OCAML_VERSION >= (5, 2, 0)
    let auto_include = Load_path.no_auto_include in
    let visible = paths.Load_path.visible in
    let hidden = paths.Load_path.hidden in
    Load_path.init ~auto_include ~visible ~hidden
    #elif OCAML_VERSION >= (5, 0, 0)
    let auto_include = Load_path.no_auto_include in
    Load_path.init ~auto_include paths
    #else
    Load_path.init paths
    #endif

  let set_loadpaths paths =
    let reset () =
      init_load_path paths;
      Envaux.reset_cache ()
    in
    setup := Lazy.from_fun reset

  let load_env env =
    force_setup ();
    Envaux.env_of_only_summary env
end

module Compat = struct

  open Typedtree

  (* Conversions *)

  let unlabel_tuple fields =
    #if OCAML_VERSION >= (5, 4, 0)
    List.map snd fields
    #else
    fields
    #endif

  let options_of_args args =
    #if OCAML_VERSION >= (5, 4, 0)
    (* Texp_apply's args changed in OCaml 5.4, from expression option
       to arg_or_omitted. This does the reverse conversion *)
    let args =
      List.map
        (fun (lab, arg) ->
          match arg with
          | Arg expr -> lab, Some expr
          | Omitted _ -> lab, None
        )
        args
    in
    #endif
    args

  (* Getters *)

  #if OCAML_VERSION < (5, 2, 0)
  let dummy_uid = Shape.Uid.internal_not_actually_unique
    (* A uid field appears in multiple constructors in OCaml 5.2.
       This dummy value serves as replacement. We do not rely on its value
       but need it to exist for typing. *)
  #endif

  type alias_data = value general_pattern * Ident.t * Location.t * Shape.Uid.t

  let get_alias_data : type k . k pattern_desc -> alias_data option = function
    #if OCAML_VERSION >= (5, 4, 0)
    | Tpat_alias (pat, id, {loc; _}, uid, _) ->
    #elif OCAML_VERSION >= (5, 2, 0)
    | Tpat_alias (pat, id, {loc; _}, uid) ->
    #else
    | Tpat_alias (pat, id, {loc; _}) ->
        let uid = dummy_uid in
    #endif
      Some (pat, id, loc, uid)
    | _ -> None

  type var_data = Ident.t * string Location.loc * Shape.Uid.t

  let get_var_data : type k . k pattern_desc -> var_data option = function
    (* x *)
    #if OCAML_VERSION >= (5, 2, 0)
    | Tpat_var (id, loc, uid) ->
    #else
    | Tpat_var (id, loc) ->
        let uid = dummy_uid in
    #endif
        Some (id, loc, uid)
    (* (x: t) *)
    #if OCAML_VERSION >= (5, 4, 0)
    | Tpat_alias ({pat_desc=Tpat_any; _}, id, loc, uid, _) ->
    #elif OCAML_VERSION >= (5, 2, 0)
    | Tpat_alias ({pat_desc=Tpat_any; _}, id, loc, uid) ->
    #else
    | Tpat_alias ({pat_desc=Tpat_any; _}, id, loc) ->
        let uid = dummy_uid in
    #endif
        Some (id, loc, uid)
    | _ -> None

  type match_data =
    expression * computation case list * value case list * partial

  let get_match_data = function
    #if OCAML_VERSION >= (5, 3, 0)
    | Texp_match (exp, reg_cases, eff_cases, partial) ->
    #else
    | Texp_match (exp, reg_cases, partial) ->
        let eff_cases = [] in (* effect cases appear in OCaml 5.3 *)
    #endif
        Some (exp, reg_cases, eff_cases, partial)
    | _ -> None

  type try_data =
    expression * value case list * value case list

  let get_try_data = function
    #if OCAML_VERSION >= (5, 3, 0)
    | Texp_try (exp, reg_cases, eff_cases) ->
    #else
    | Texp_try (exp, reg_cases) ->
        let eff_cases = [] in (* effect cases appear in OCaml 5.3 *)
    #endif
        Some (exp, reg_cases, eff_cases)
    | _ -> None

  type function_bodies = expression list

  let get_function_bodies = function
    #if OCAML_VERSION >= (5, 2, 0)
    | Texp_function (_, Tfunction_body expr) -> expr::[]
    | Texp_function (_, Tfunction_cases { cases ; _ }) ->
    #else
    | Texp_function {cases ; _} ->
    #endif
        List.map (fun {c_rhs; _} -> c_rhs) cases
    | _ -> []

end
