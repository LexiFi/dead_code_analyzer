type t = (Lexing.position * Lexing.position) list
    (** Dependencies similar to [cmt_infos.cmt_value_dependencies] in OCaml 5.2 *)

val empty : t (** No signature read *)

val init : Cmt_format.cmt_infos option -> Cmt_format.cmt_infos option -> (t, string) result
(** [init cmt_infos cmti_infos] expects [cmt_infos = Some _].
    It reads the [cmt_infos] and the |cmti_infos] to retrieve their
    [cmt_declaration_dependencies] and convert them into a single [t].
    It returns an [Ok t] with [t] on success.
    In case the [cmt_infos] is missing or does not contain an implementation,
    it returns an [Err msg] with msg a string describing the issue. *)
