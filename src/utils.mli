module Filepath : sig

  type t = string

  val remove_pp : t -> t
  (** [remove_pp filepath] removes the `.pp` extension (if it exists) from
      [filepath]. Eg. [remove_pp "dir/foo.pp.ml" = "dir/foo.ml"] *)

  val unit : t -> string
  (** [unit filepath] estimates the compilation unit of [filepath] *)

  type kind =
    | Cmti (** .cmti file *)
    | Cmt_without_mli (** .cmt file of .ml only module *)
    | Cmt_with_mli (** .cmt file of module with .mli *)
    | Dir (** Directory *)
    | Ignore (** Irrelevant for the analyzer *)

  val kind : exclude:(t -> bool) -> t -> kind
  (** [kind ~exclude filepath] returns the kind of [filepath].
      If [exclude filepath = true], [filepath] does not exists, or [filepath]
      does not fit in another kind, then its kind is [Ignore].
      Other kinds are self explanatory. *)
end

val signature_of_modtype :
  ?select_param:bool -> Types.module_type -> Types.signature
(** [signature_of_modtype ?select_param modtype] returns the selected signature
    of [modtype]. If [modtype] is a functor, then [select_param] is used to
    select either the signature of the parameter or the result of the functor.
    Note: [select_param] is [false] by default. If set to [true], it is reset to
          [false] after looking for the parameter of the first functor.
          There is currently no way to select the parameter of a parameter.  *)

val typedtree_signature_of_modtype :
  ?select_param:bool -> Typedtree.module_type -> Typedtree.signature option
(** [signature_of_modtype ?select_param modtype] returns the selected
    Typedtree.signature of [modtype] when possible.
    See {!signature_of_modtype} above for more information
*)

module StringSet : Set.S with type elt = String.t

module Envaux : sig
  type paths =
    #if OCAML_VERSION >= (5, 2, 0)
    Load_path.paths
    #else
    string list
    #endif

  val set_loadpaths : paths -> unit
  (** Reset the load_path to the [paths]. Also calls Envaux.reset_cache.
      To call when loading a new .cmt *)

  val load_env : Env.t -> Env.t
  (** Same as Envaux.env_of_only_summary but ensures the paths submitted
      in set_loadpaths are actually set. *)
end

module Compat : sig

  open Typedtree

  #if OCAML_VERSION >= (5, 4, 0)
  val unlabel_tuple : ('a * 'b) list -> 'b list
  #else
  val unlabel_tuple : 'a list -> 'a list
  #endif
    (** Tuple's field representation changed in OCaml 5.4, with the
        introduction of labelled tuples. This converts a tuple's fields back
        into the pre-5.4 representation. *)

val options_of_args :
  #if OCAML_VERSION >= (5, 4, 0)
  (Asttypes.arg_label * (expression, unit) arg_or_omitted) list
  #else
  (Asttypes.arg_label * expression option) list
  #endif
  -> (Asttypes.arg_label * expression option) list
    (** Apply's arguments representation changed in OCaml 5.4, from
        expression option to arg_or_omitted. This does the reverse conversion *)

  type _ invalid_arg =
    | Unexpected_pattern : string * 'k pattern_desc -> 'k pattern_desc invalid_arg
        (** Used by pattern_desc getters *)
    | Unexpected_expression :
        string * expression_desc -> expression_desc invalid_arg
        (** Used by expression_desc getters *)

  type ('k, 'a) pat_getter = 'k pattern_desc -> ('a, 'k pattern_desc invalid_arg) result

  type alias_data = value general_pattern * Ident.t * Location.t * Shape.Uid.t

  val get_alias_data : 'k . ('k, alias_data) pat_getter
    (** [get_alias_data pat] expects a [Tpat_alias].
        It returns [Error Unexpected_pattern] otherwise.
        It extracts the different components of Tpat_alias.
        If OCaml < 5.2, the [Uid.t] field is a dummy value.
        If OCaml >= 5.4, the last field of Tpat_alias is discarded.
    *)

  val get_alias_data_exn : 'k . 'k pattern_desc -> alias_data
    (** same as [get_alias_data] but raises instead of returning an Error *)

  type var_data = Ident.t * string Location.loc * Shape.Uid.t

  val get_var_data : 'k . ('k, var_data) pat_getter
    (** [get_var_data pat] expects a [Tpat_var] or a [Tpat_alias(Tpat_any)].
        It returns [Error Unexpected_pattern] otherwise.
        It extracts the different components of Tpat_var.
        If OCaml < 5.2, the [Uid.t] field is a dummy value.
        [Tpat_alias(Tpat_any)] is considered equivalent to a [Tpat_var]
        because in OCaml < 5.5, constrained vars ([x : t]) are translated
        in this pattern.
    *)

  val get_var_data_exn : 'k . 'k pattern_desc -> var_data
    (** same as [get_var_data] but raises instead of returning an Error *)

  type 'a exp_getter =
    expression_desc -> ('a, expression_desc invalid_arg) result

  type match_data =
    expression * computation case list * value case list * partial

  val get_match_data : match_data exp_getter
    (** [get_match_data exp] expects a [Texp_match].
        It returns [Error Unexpected_expression] otherwise.
        Otherwise it extracts the different components of Texp_match.
        If OCaml < 5.3, the [value case list] field is en empty list.
    *)

  val get_match_data_exn : expression_desc -> match_data
    (** same as [get_match_data] but raises instead of returning an Error *)

  type try_data = expression * value case list * value case list

  val get_try_data : try_data exp_getter
    (** [get_try_data exp] expects a [Texp_try].
        It returns [Error Unexpected_expression] otherwise.
        Otherwise it extracts the different components of Texp_try.
        If OCaml < 5.3, the second [value case list] field is en empty list.
    *)

  val get_try_data_exn : expression_desc -> try_data
    (** same as [get_try_data] but raises instead of returning an Error *)

  type function_bodies = expression list

  val get_function_bodies : function_bodies exp_getter
    (** [get_function_bodies exp] expects a [Texp_function].
        It returns [Error Unexpected_expression] otherwise.
        Otherwise it returns the expression of the function body.
        If there are multiple cases, then the returned list contains all the
        alternative expressions.
    *)

end
