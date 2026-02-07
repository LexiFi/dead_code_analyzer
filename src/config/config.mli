(** Configuration of the analyzer *)

(** {2 Sections configuration} *)

module Sections = Sections

val must_report_section : _ Sections.section -> bool
(** [must_report_section sec] returns `true` if the section must be reported *)

val must_report_call_sites : _ Sections.section -> bool
(** [must_report_call_sites sec] returns `true` if call sites must be reported in
    thresholded subsections *)

val get_main_threshold : Sections.main_section -> int
(** [get_main_threshold main_sec] returns the threshold if
    [main_sec = Threshold _], [0] otherwise. *)

(** {2 General configuration} *)

type t = private
  { verbose : bool (** Display additional information during the analaysis *)
  ; internal : bool (** Keep track of internal uses for exported values *)
  ; underscore : bool (** Keep track of elements with names starting with [_] *)
  ; paths_to_analyze : Utils.StringSet.t
      (** Cmi and cmt filepaths found by exploring the paths provided in the
          command line and considered for analysis *)
  ; excluded_paths : Utils.StringSet.t
      (** Cmi and cmt filepaths to exclude from the analysis *)
  ; references_paths : Utils.StringSet.t
      (** Cmi and cmt filepaths to explore for references only *)
  ; sections : Sections.t (** Config for the different report sections *)
  }

val default_config : t
(** Default configuration for the analysis.
    By default [verbose], [internal], and [underscore] are [false]
    By default [paths_to_analyze], [excluded_paths], and [references_paths] are empty.
    By default [sections] is [Sections.default] *)

val must_report_main : t -> bool
(** [must_report_main config] indicates if any of the main sections
    is activated in [config] *)

val must_report_opt_args : t -> bool
(** [must_report_opt_args config] indicates if any of the optional
    arguments section is activated in [config] *)

val update_style : string -> t -> t
(** [update_style arg config] returns a [config] with style section
    configuration updated according to the [arg] specification. *)

val is_excluded : string -> t -> bool
(** [is_excluded path config] indicates if [path] is excluded from the analysis
    in [config].
    Excluding a path is done with the --exclude command line argument. *)

val parse_cli : unit -> t
(** [parse_cli ()] returns a fresh configuration filled up according to the
    command line arguments *)
