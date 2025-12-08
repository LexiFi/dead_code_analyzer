type t = {
  uid_to_loc : Lexing.position Shape.Uid.Tbl.t;
    (** Maps from uids found in the [cmt]'s structure and the
        [cmt_uid_decl_to_dep] fields in [.cmt] and [.cmti] to their locations
        in source files. *)
}

val empty : unit -> t (** No signature read *)

val init : File_infos.t -> (t, string) result
(** [init file_infos] expects [file_infos] to be properly [Filen_infos.init]'ed.
    It reads the [cmt_infos] to fill out the [uid_to_loc] table.
    It returns an [Ok t] with [t] on success.
    In case the [cmt_infos] is missing, it returns an [Err msg] with msg a string
    describing the issue. *)
