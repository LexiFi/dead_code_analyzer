(** Stateful info of the analysis *)

module File_infos = File_infos

type t =
  { config : Config.t (** Configuration of the analysis *)
  ; file_infos : File_infos.t (** Info about the file being analyzed *)
  ; signature : Signature.t
      (** Data extracted or built using the current compialtion unit's
          signature *)
  }

val init : Config.t -> t
(** [init config] initial state for an analysis configured by [config] *)

val update_config : Config.t -> t -> t
(** [update_config config state] changes the analysis configuration *)

val change_file : t -> string -> (t, string) result
(** [change_file t cmti_file] prepare the analysis to move on to [cmti_file].
    See [File_infos.change_file] for error cases. *)

val get_current : unit -> t
(** [get_current ()] returns the state used during the analysis. *)

val update : t -> unit
(** [update t] replaces the analysis' state with [t]. *)
