(** Stateful info of the analysis *)

module File_infos = File_infos

type t = {
  file_infos : File_infos.t; (** Info about the file being analyzed *)
}

val empty : t (** The empty state *)

val init : string -> (t, string) result
(** [init cmti_file] initialize a state to analyze [cmti_file].
    See the fields respective [init]s for error cases. *)

val change_file : t -> string -> (t, string) result
(** [cahnge_file t cmti_file] prepare the analysis to move on to [cmti_file].
    See [File_infos.change_file] for error cases. *)

val get_current : unit -> t
(** [get_current ()] returns the state used during the analysis. *)

val update : t -> unit
(** [update t] replaces the analysis' state with [t]. *)
